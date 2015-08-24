
-- | TAG a stack project based on snapshot versions

{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Stack.Tag where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

import qualified Control.Exception as E
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Traversable as T

import Control.Monad.Reader
import Data.Maybe
import Data.Text (Text)
import System.Directory
import System.Process
import System.Exit

import Control.Concurrent.Async.Pool

data StackTagOpts
  = StackTagOpts
    {
      -- | Location of the stack.yaml to generate
      -- tags for
      optsStackYaml :: !(Maybe FilePath)

      -- | Verbose output
    , optsVerbose   :: !Bool

      -- | Flag to ignore any cached tags and re-run the tagger
    , noCache       :: !Bool

      -- TODO flags

      -- set tag command, hasktags, hothasktags, etc
      -- , tagCommand :: !Tagger
      -- Set format etags/ctags
      -- , tagFormat  :: !TagFmt
      -- Only tag dependencies explicitly mentioned in
      -- the cabal file.
      -- , noTransitive :: !Bool
    } deriving Show


data Tagger = Hasktags
            | HotHasktags
            | OtherTagger Text
            deriving Show

data TagFmt = CTag
            | ETag
            | OtherFmt Text
            deriving Show

type TagOutput = FilePath
type SourceDir = FilePath
data TagCmd = TagCmd Tagger TagFmt TagOutput SourceDir deriving Show

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
           sources     <- stkPaths
           depSources  <- stkDepSources
           tagSources sources depSources

--------------------------------------------------------------------------
--------------------------------------------------------------------------

io :: MonadIO m => IO a -> m a
io = liftIO

p :: (MonadIO m) => String -> m ()
p = io . putStrLn

-- | Run a command using the `stack' command-line tool
-- with a list of arguments
runStk :: [String] -> IO (ExitCode, String, String)
runStk args
  = readProcessWithExitCode "stack" args []

--- | Check whether the current version of stack
-- is compatible by trying to run `stack list-depenencies --help`.
chkStackCompatible :: StackTag ()
chkStackCompatible = do
  (exitc,out,_) <- io $ runStk ["list-dependencies", "--help"]
  case exitc of
    ExitSuccess    -> return ()
    ExitFailure _e ->
      p (show exitc) >> error "You need stack version 0.1.2.2 or higher installed and in your PATH to use stack-tag"

-- | Get a list of relavant directories from stack using
-- the @stack path@ command
stkPaths :: StackTag [(Text,[Text])]
stkPaths = do
    (_,ps,_) <- io $ runStk ["path"]
    return (parsePaths ps)
  where
    parsePaths = map parsePath . T.lines . T.pack
    parsePath ps = let (k,vs) = T.breakOn ":" ps
                   in (k, splitAndStrip vs)
    splitAndStrip = filter (not . T.null) . map T.strip . T.splitOn ":"

-- | Get a list of dependencies using:
-- @stack --list-dependencies --separator=-@
stkDepSources :: StackTag [String]
stkDepSources = do
  (_exitc,ls,_) <- io $ runStk ["list-dependencies", "--separator=-"]
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
  let cmd = TagCmd Hasktags ETag "stack.tags" tagroot
  p $ "Running Tagger => " ++ show cmd

  thisProj <- io $ runTagger cmd

  p "Merging TAGS"
  taggedSrcs <- T.traverse (io . readFile) (catMaybes (thisProj : depTagFiles))
  let xs = concat $ fmap lines taggedSrcs
      ys = if False then (Set.toList . Set.fromList) xs else xs
  io $ writeFile "TAGS" $ unlines ys

parTag :: [(Text, [Text])] -> [FilePath] -> StackTag [Maybe FilePath]
parTag srcs depsrcs = do
  StackTagOpts {noCache=nocache} <- ask

  -- control the number of jobs by using capabilities Currently,
  -- capabilities creates a few too many threads which saturates the
  -- CPU and network connection. For now, it's manually set to 3 until
  -- a better threading story is figured out.
  --
  -- io $ mapCapabilityPool (tagDependency nocache srcs) depsrcs
  io $ mapPool 3 (tagDependency nocache srcs) depsrcs

-- | Tag a single dependency
tagDependency :: Bool -> [(Text, [Text])] -> FilePath -> IO (Maybe FilePath)
tagDependency nocache stkpaths dep = do

    let msg          = "No 'snapshot-install-root' found, aborting."
        fatal        = error ("Fatal error tagging " ++ dep ++ ". " ++ msg)
        (snapRoot:_) = fromMaybe fatal (lookup "snapshot-install-root" stkpaths)
        dir          = T.unpack snapRoot ++ "/packages" ++ "/" ++ dep
        tagFile      = dir ++ "/TAGS"

    -- HACK as of Aug 5 2015, `stack unpack` can only download sources
    -- into the current directory. Therefore, we move the source to
    -- the correct snapshot location. This could/should be fixed in
    -- the stack source (especially since --haddock has similar
    -- behavior). A quick solution to avoid this might be to run the
    -- entire function in the target directory
    readProcess "rm" ["--preserve-root", "-rf", dep] []

    exists <- doesDirectoryExist dir
    tagged <- doesFileExist tagFile

    if exists
        then p $ "Cached version of " ++ dep ++ " found"
        else p $ "No cached version of " ++ dep ++ " found"

    unless exists $ void $ do
      createDirectoryIfMissing True dir
      p $ "Unpacking " ++ dep
      (ec,stout,_) <- runStk ["unpack", dep]
      case ec of
        ExitFailure _ -> void $ p stout
        ExitSuccess   -> void $ do
          p $ "Moving " ++ dep ++ " to snapshot location " ++ dir
          readProcess "mv" [dep,dir] []

    if tagged && nocache
       then return (Just tagFile)
       else do p $ "Tagging " ++ dep
               runTagger (TagCmd Hasktags ETag tagFile dir)
                 `E.catch` handleError
   where
    handleError (E.SomeException err) = p (show err) >> return Nothing

runTagger :: MonadIO m => TagCmd -> m (Maybe TagOutput)
runTagger (TagCmd t fmt to fp)
  = do (ec,stout,_) <- io $ readProcessWithExitCode
                                        (tagExe t)
                                        [tagFmt fmt, "-R", "--ignore-close-implementation", "--output", to, fp]
                                        []
       case ec of
         ExitFailure _ -> p stout >> return Nothing
         ExitSuccess   -> return (Just to)

tagExe :: Tagger -> String
tagExe Hasktags   = "hasktags"
tagExe _          = error "Tag command not supported. Feel free to create an issue at https://github.com/creichert/stack-tag"

tagFmt :: TagFmt -> String
tagFmt ETag    = "--etags"
tagFmt _       = error "Tag format not supported. Feel free to create an issue at https://github.com/creichert/stack-tag"
