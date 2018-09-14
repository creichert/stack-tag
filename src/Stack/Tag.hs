{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

-- | TAG a stack project based on snapshot versions

module Stack.Tag where

import qualified Data.Set          as Set
import qualified Data.Text         as T
import qualified Data.Traversable  as T

import Control.Exception    as E
import Control.Monad.Reader
import Data.Either
import Data.Maybe
import Data.Text            (Text)
import System.Directory
import System.Exit
import System.Process

import Control.Concurrent.Async.Pool


data StackTagOpts = StackTagOpts {

      -- | Location of the stack.yaml to generate tags for
      optsStackYaml :: !(Maybe FilePath)

      -- | Verbose output
    , optsVerbose   :: !Bool

      -- | Flag to ignore any cached tags and re-run the tagger
    , noCache       :: !Bool

    } deriving Show


data Tagger = Hasktags
            | HotHasktags
            | OtherTagger Text
            deriving Show

data TagFmt = CTags
            | ETags
            | Both
            | OtherFmt Text
            deriving Show

type TagOutput = FilePath
type SourceDir = FilePath
type PkgName = String

data TagCmd = TagCmd Tagger TagFmt TagOutput SourceDir PkgName deriving Show

newtype StackTag a = StackTag {
    runStackTag :: ReaderT StackTagOpts IO a
  } deriving (
               Functor
             , Applicative
             , Monad
             , MonadReader StackTagOpts
             , MonadIO
             )

defStackOpts :: StackTagOpts
defStackOpts = StackTagOpts Nothing False True

stackTag :: StackTagOpts -> IO ()
stackTag = runReaderT (runStackTag app)
 where
  app = do chkStackCompatible
           chkHaskTags
           chkIsStack
           sources     <- stkPaths
           depSources  <- stkDepSources
           tagSources sources depSources

--------------------------------------------------------------------------
--------------------------------------------------------------------------

io :: MonadIO m => IO a -> m a
io = liftIO

p :: String -> StackTag ()
p msg = whenM (ask >>= pure . optsVerbose) $ io (putStrLn msg)

whenM :: Monad m => m Bool -> m () -> m ()
whenM predicate go = predicate >>= flip when go

-- | Run a command using the `stack' command-line tool
-- with a list of arguments
runStk :: [String] -> StackTag (ExitCode, String, String)
runStk args = io $ readProcessWithExitCode "stack" args []

chkIsStack :: StackTag ()
chkIsStack = do
  StackTagOpts {optsStackYaml=stackYaml} <- ask
  sYaml <- io $ doesFileExist "stack.yaml"

  case stackYaml of
    Nothing -> unless sYaml $ error "stack.yaml not found or specified!"
    _       -> return ()

chkHaskTags :: StackTag ()
chkHaskTags = do
  ht <- io $ findExecutable "hasktags"
  case ht of
    Just _p -> return ()
    Nothing -> error "You must have hasktags installed Run 'stack install hasktags'."

--- | Check whether the current version of stack
-- is compatible by trying to run `stack list-depenencies --help`.
chkStackCompatible :: StackTag ()
chkStackCompatible = do
  (exitc, _, _) <- runStk ["ls", "dependencies", "--help"]
  case exitc of
    ExitSuccess    -> return ()
    ExitFailure _e ->
      p (show exitc) >> error "You need stack version 1.7.1 or higher installed and in your PATH to use stack-tag"

-- | Get a list of relavant directories from stack using
-- the @stack path@ command
stkPaths :: StackTag [(Text,[Text])]
stkPaths = do
    (_,ps,_) <- runStk ["path"]
    return (parsePaths ps)
  where
    parsePaths = map parsePath . T.lines . T.pack
    parsePath ps = let (k,vs) = T.breakOn ":" ps
                   in (k, splitAndStrip vs)
    splitAndStrip = filter (not . T.null) . map T.strip . T.splitOn ":"

-- | Get a list of dependencies using:
-- @stack --list-dependencies --test --bench --separator=-@
stkDepSources :: StackTag [String]
stkDepSources = do
  (_exitc,ls,_) <- runStk [ "ls", "dependencies", "--external"
                               , "--include-base", "--test"
                               , "--bench", "--separator=-"]
  return $ lines ls

--------------------------------------------------------------------------
--------------------------------------------------------------------------

tagSources :: [(Text,[Text])] -> [FilePath] -> StackTag ()
tagSources srcs depsrcs = do

  let srcDir    = lookup "project-root" srcs
  let tagroot   = T.unpack . fromMaybe "." . listToMaybe . fromMaybe [] $ srcDir

  -- alternative pooled
  depTagFiles <- parTag srcs depsrcs

  -- map a tag command over all provided sources
  thisProj   <- runTagger (TagCmd Hasktags ETags "stack.tags" tagroot "project-root")
  taggedSrcs <- T.traverse (io . readFile) (rights (thisProj : depTagFiles))

  let errors = lefts (thisProj : depTagFiles)
  unless (null errors) $ do

      let pkg_errs = map (\(pkg,err) -> pkg ++ ": " ++ err) $ take 10 errors
      error $ unlines $
                   "[tag:error] stack-tag encountered errors creating tags"
                 : pkg_errs

  let xs = concatMap lines taggedSrcs
      ys = if False then (Set.toList . Set.fromList) xs else xs

  p $ "[tag:all] merged tags " ++ show (length taggedSrcs) ++ " projects"
  io $ writeFile "TAGS" $ unlines ys

parTag :: [(Text, [Text])] -> [FilePath] -> StackTag [Either (PkgName, String) FilePath]
parTag srcs depsrcs = do

  o@StackTagOpts {noCache=nocache} <- ask

  -- control the number of jobs by using capabilities Currently,
  -- capabilities creates a few too many threads which saturates the
  -- CPU and network connection. For now, it's manually set to 3 until
  -- a better threading story is figured out.
  --
  -- io $ mapCapabilityPool (tagDependency nocache srcs) depsrcs

  -- WAT: rewrap the transformer. It's less heavy duty than bringing in
  -- monad-control or refactoring (2018-09-14)
  let worker osrc = flip runReaderT o $ do
          runStackTag (tagDependency nocache srcs osrc)

  io $ mapPool 3 worker depsrcs

-- | Tag a single dependency
tagDependency :: Bool -> [(Text, [Text])] -> FilePath -> StackTag (Either (PkgName, String) FilePath)
tagDependency nocache stkpaths dep = do

    let snapRoot
            | Just (sr : _) <- lookup "snapshot-install-root" stkpaths = sr
            | otherwise = error ("[tag:error] error tagging "
                               ++ dep
                               ++ ". "
                               ++ "No 'snapshot-install-root' found, aborting.")
        dir          = T.unpack snapRoot ++ "/packages" ++ "/" ++ dep
        tagFile      = dir ++ "/TAGS"

    -- HACK as of Aug 5 2015, `stack unpack` can only download sources
    -- into the current directory. Therefore, we move the source to
    -- the correct snapshot location. This could/should be fixed in
    -- the stack source (especially since --haddock has similar
    -- behavior). A quick solution to avoid this might be to run the
    -- entire function in the target directory
    _ <- io $ readProcess "rm" ["--preserve-root", "-rf", dep] []

    -- error (show dep)

    exists <- io $ doesDirectoryExist dir
    tagged <- io $ doesFileExist tagFile

    unless exists $ void $ do
        io $ createDirectoryIfMissing True dir
        p $ "[tag:download] " ++ dep
        (ec,stout,_) <- runStk ["unpack", dep]
        case ec of
          ExitFailure _ -> void $ do
              p $ "[tag:download] failed to download " ++ dep ++ " - " ++ stout
          ExitSuccess   -> void $ do
              p $ "[tag:download] cp " ++ dep ++ " to snapshot source cache in " ++ dir
              io $ readProcess "mv" [dep,dir] []

    if tagged && nocache
       then do p $ "[tag:cache] " ++ dep
               return (Right tagFile)
       else do p $ "[tag:nocache] " ++ dep
               runTagger (TagCmd Hasktags ETags tagFile dir dep)

runTagger :: TagCmd -> StackTag (Either (PkgName, String) TagOutput)
runTagger (TagCmd t fmt to fp dep) = do

    let opts = [ tagFmt fmt
               , "-R"  -- tags-absolute
               -- made the default & removed
               -- , "--ignore-close-implementation"
              , "--follow-symlinks"
              -- , "--cache"
              , "--output"
              , to
              , fp
              ]

    (ec, stout, err) <- hasktags opts

    case ec of
        ExitFailure _
            | null err -> return $ Left (dep, stout)
            | otherwise -> return $ Left (dep, err)
        ExitSuccess   -> return $ Right to
  where
    hasktags opts = io $
        readProcessWithExitCode (tagExe t) opts []
          `E.catch` (\(SomeException err) -> return (ExitFailure 1, displayException err, ""))


-- TODO tagExe Hasktags   = "fast-tags"
tagExe :: Tagger -> String
tagExe Hasktags   = "hasktags"
tagExe _          = error "Tag command not supported. Feel free to create an issue at https://github.com/creichert/stack-tag"

tagFmt :: TagFmt -> String
tagFmt ETags   = "--etags"
tagFmt CTags   = "--etags"
tagFmt Both    = "--both"
tagFmt _       = error "Tag format not supported. Feel free to create an issue at https://github.com/creichert/stack-tag"
