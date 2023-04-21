{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}

-- | TAG a stack project based on snapshot versions
module Stack.Tag (
      StackTagOpts(..)
    , stackTag
    ) where

import qualified Data.Set         as Set
import qualified Data.Text        as T
import qualified Data.Traversable as T
import qualified Hasktags
import qualified GHC.IO.IOMode as GHC

import Control.Monad.Reader
import Data.Either
import Data.Maybe
import Data.Text            (Text)
import System.Directory
import System.Exit
import System.Process

import Control.Concurrent.Async.Pool

import qualified Debug.Trace as Trace


data StackTagOpts = StackTagOpts {

      -- | Verbose output
      optsVerbose :: !Bool

      -- | Flag to ignore any cached tags and re-run the tagger
    , noCache     :: !Bool

    } deriving Show


data Tagger = Hasktags
            | HotHasktags
            | OtherTagger Text
            deriving Show

data TagFmt = CTags
            | ETags
            | Both
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


-- | Generate tags for a stack project indicated by a `stack.yaml` in the current directory.
stackTag :: StackTagOpts -> IO ()
stackTag = runReaderT (runStackTag app)
  where
    app = do
        chkStackCompatible
        chkIsStack
        paths <- stkPaths
        depSources <- stkDepSources
        tagSources paths depSources

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
    stack_yaml <- io $ doesFileExist "stack.yaml"
    unless stack_yaml $ error "stack.yaml not found or specified!"

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
stkPaths :: StackTag [(Text, [Text])]
stkPaths = do
    (_, ps, _) <- runStk ["path"]
    return (parsePaths ps)
  where
    parsePaths = map parsePath . T.lines . T.pack
    parsePath ps =
        let (k, vs) = T.breakOn ":" ps
        in (k, splitAndStrip vs)
    splitAndStrip = filter (not . T.null) . map T.strip . T.splitOn ":"

-- | Get a list of dependencies using:
stkDepSources :: StackTag [FilePath]
stkDepSources = do
    (_exitc,ls,_) <- runStk [ "ls"
                            , "dependencies"
                            , "--separator=-"
                            , "--external"
                            , "--include-base"
                            , "--prune", "ghc-boot-th,rts"
                            -- , "--test"
                            -- , "--bench"
                            ]
    return $ fmap T.unpack
           $ T.lines
           $ T.pack ls

--------------------------------------------------------------------------
--------------------------------------------------------------------------

tagSources :: [(Text,[Text])] -> [FilePath] -> StackTag ()
tagSources srcs depsrcs = do

  let srcDir    = lookup "project-root" srcs
  let tagroot   = T.unpack . fromMaybe "." . listToMaybe . fromMaybe [] $ srcDir

  -- alternative pooled
  depTagFiles <- parTag srcs depsrcs

  -- map a tag command over all provided sources
  dir <- io getCurrentDirectory
  thisProj   <- runTagger (TagCmd Hasktags ETags "stack.tags" tagroot dir)
  taggedSrcs <- T.traverse (io . readFile) (rights (thisProj : depTagFiles))

  let errors = lefts (thisProj : depTagFiles)
  unless (null errors) $ do

      let pkg_errs = map (\(pkg,err) -> pkg ++ ": " ++ err) $ take 10 errors
      error $ unlines $
                   "[tag:error] stack-tag encountered errors creating tags"
                 : pkg_errs

  let xs = concatMap lines taggedSrcs
      ys = if False then (Set.toList . Set.fromList) xs else xs

  p $ "[tag:done] built & merged tags for " ++ show (length taggedSrcs) ++ " projects"
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

    -- HACK as of Aug 5 2015, `stack unpack` can only download sources
    -- into the current directory. Therefore, we move the source to
    -- the correct snapshot location. This could/should be fixed in
    -- the stack source (especially since --haddock has similar
    -- behavior). A quick solution to avoid this might be to run the
    -- entire function in the target directory
    -- _ <- io $ readProcess "rm" ["--preserve-root", "-rf", dep] []

    unpack nocache dep stkpaths >>= \case

        Nothing ->
            return (Left (dep, ""))

        Just (dir, tagFile, tagged) -> do

            if tagged && not nocache then do

               p $ "[tag:nocache] " ++ dep
               return (Right tagFile)

            else do
               p $ "[tag:cache] " ++ dep
               runTagger (TagCmd Hasktags ETags tagFile dir dep)

unpack :: Bool
       -> String
       -> [(Text , [Text])]
       -> StackTag (Maybe (String, String, Bool))
unpack nocache dep stkpaths = do

    let snapRoot
            | Just (sr : _) <- lookup "snapshot-install-root" stkpaths = sr
            | otherwise = error ("[tag:error] error tagging "
                               ++ dep
                               ++ ". "
                               ++ "No 'snapshot-install-root' found, aborting.")
        all_deps_dir = T.unpack snapRoot ++ "/packages"
        dep_dir      = all_deps_dir ++ "/" ++ dep
        tagFile      = dep_dir ++ "/TAGS"

    exists <- io $ doesDirectoryExist dep_dir
    tagged <- io $ doesFileExist tagFile

    if not nocache && exists then do
        return $ Just (dep_dir, tagFile, tagged)
    else do
        p $ "[tag:download] " ++ dep
        (ec,_,sterr) <- runStk ["unpack"
                               , "--to", all_deps_dir
                               , dep
                               ]
        case ec of
          ExitFailure _
              | exists -> do
                    return $ Just (dep_dir, tagFile, tagged)
              | otherwise -> do
                    p $ "[tag:download] failed to download " ++ dep ++ " - " ++ sterr
                    return Nothing
          ExitSuccess -> do
              return $ Just (dep_dir, tagFile, tagged)

runTagger :: TagCmd -> StackTag (Either (PkgName, String) TagOutput)
runTagger (TagCmd _ fmt to dir dep) = do

    -- let is_taggable src
    --       | "{-# CTYPE #-}" `T.isInfixOf` src -> False
    --       | "test/" `T.isPrefixOf src -> False
    -- findFileWith isHaskellSrc dir Executable path (binary <.> exeExtension)

    filepaths <- io $ Hasktags.dirToFiles True [".hs"] dir

    let opts = Hasktags.Mode {
                 Hasktags._tags = case fmt of
                                      CTags -> Hasktags.Ctags
                                      ETags  -> Hasktags.Etags
                                      Both -> undefined -- Hasktags.Both
               , Hasktags._extendedCtag = False
               , Hasktags._appendTags = GHC.WriteMode -- AppendMode
               , Hasktags._outputFile = Hasktags.TagsFile {
                                          Hasktags._ctagsFile = to
                                        , Hasktags._etagsFile = to
                                        }
               , Hasktags._cacheData = False
               , Hasktags._followSymlinks = True
               , Hasktags._suffixes = [".hs",".lhs"]
               , Hasktags._absoluteTagPaths = True
               }

    p $ "[tag:tagging] " ++ dep
    (ec, stout, err) <- io $ hasktags opts filepaths -- `E.catch` onerr

    case ec of
        ExitFailure _
            | null err -> return $ Left (dep, stout)
            | otherwise -> return $ Left (dep, err)
        ExitSuccess -> return $ Right to
  where
    hasktags opts filepaths = do
        Hasktags.generate opts filepaths
        return (ExitSuccess, "", "")
    -- onerr (SomeException err) = return (ExitFailure 1, displayException err, "")
