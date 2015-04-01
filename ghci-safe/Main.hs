{-# OPTIONS -fno-warn-incomplete-patterns -optc-DNON_POSIX_SOURCE #-}

-----------------------------------------------------------------------------
--
-- GHC Driver program
--
-- (c) The University of Glasgow 2005
--
-----------------------------------------------------------------------------

module Main (main) where

import InteractiveUI
import GhciMonad        ( GHCi )

import qualified GHC

import CmdLineParser    ( CmdLineP(), Flag() )
import Config           ( cBooterVersion, cProjectVersion, cStage )
import DriverPhases     ( Phase(..), isSourceFilename, startPhase )
import DynFlags         ( defaultFatalMessager, defaultFlushOut, gopt_set,
                          flagsAll, ghcMode, ghcLink, hscTarget,
                          parseDynamicFlagsFull, verbosity )
import HscTypes         ( handleFlagWarnings, handleSourceError )
import MonadUtils       ( liftIO )
import Packages         ( dumpPackages )
import StaticFlags      ( staticFlags )
import Util             ( looksLikeModuleName )

import Control.Monad
import Data.List
import Data.Maybe
import System.Environment
import System.Exit
import System.FilePath
import System.IO

data GHCiOptions = GHCiOptions {
      showStartupMsg      :: Bool,
      defaultVerbosity    :: Int,
      maxVerbosity        :: Int,
      allowFileInput      :: Bool,
      parseTopDir         :: Bool,
      defaultTopDir       :: Maybe FilePath,
      allowedDynFlags     :: Maybe [Flag (CmdLineP GHC.DynFlags)],
      allowedStaticFlags  :: Maybe [Flag IO],
      allowedGhciCommands :: Maybe [String],
      defaultDynFlags     :: [String],
      shortHelp           :: Maybe String,
      longHelp            :: Maybe String,
      startup             :: GHCi ()
   }

main :: IO ()
main = ghciMain opts
   where opts = GHCiOptions {
         showStartupMsg      = False,
         defaultVerbosity    = 0,
         maxVerbosity        = 0,
         allowFileInput      = False,
         parseTopDir         = False,
         defaultTopDir       = Just "/usr/local/lib/ghc-7.8.4/",
         allowedDynFlags     = Just [],
         allowedStaticFlags  = Just [],
         allowedGhciCommands = Just ["issafe", "type", "browse", "browse!", "kind", "kind!", "sprint", "print", "?", "help"],
         defaultDynFlags     = ["-XSafe", "-fpackage-trust", "-distrust-all-packages", "-trust base", "-XNoImplicitPrelude"],
         shortHelp           = Nothing,
         longHelp            = Just "GHCi Online...\n",
         startup             = loadNoIO
      }

-----------------------------------------------------------------------------
-- GHC's initialization routine

-- | ghciMain runs GHCi with the options specified.
ghciMain :: GHCiOptions -> IO ()
ghciMain opts = do
   hSetBuffering stdout NoBuffering
   hSetBuffering stderr NoBuffering
   GHC.defaultErrorHandler defaultFatalMessager defaultFlushOut (startGHCi opts)

-- | startGHCi starts up GHCi.
startGHCi :: GHCiOptions -> IO ()
startGHCi opts = do
   ---------------- Flag / Option configuration ---------------
   (topDir, flags, staticWarns) <- parseCmdLine opts

   GHC.runGhc topDir $ do

   dflags0 <- GHC.getSessionDynFlags

   let dflags1 = dflags0{ ghcMode    = GHC.CompManager,
                          hscTarget  = GHC.HscInterpreted,
                          ghcLink    = GHC.LinkInMemory,
                          verbosity  = defaultVerbosity opts
                        } `gopt_set` GHC.Opt_ImplicitImportQualified
       defFlags = map (GHC.mkGeneralLocated "on the commandline")
                      (defaultDynFlags opts)
       activeFlags = fromMaybe flagsAll $ allowedDynFlags opts

   -- parse default flags
   (dflags2, file_args1, dynWarns1)
      <- parseDynamicFlagsFull flagsAll True dflags1 defFlags

   -- parse dynamic command line flags
   (dflags3, file_args2, dynWarns2)
      <- parseDynamicFlagsFull activeFlags True dflags2 flags

   let dflags4     = dflags3{ verbosity = min (verbosity dflags3)
                                              (maxVerbosity opts) }
       exitOnErr e = GHC.printException e >> liftIO (exitWith $ ExitFailure 1)
       flagWarns   = staticWarns ++ dynWarns1 ++ dynWarns2

   ---------------- Initialize session ---------------
   GHC.prettyPrintGhcErrors dflags4 $ do

   handleSourceError exitOnErr $ liftIO $ handleFlagWarnings dflags4 flagWarns

   -- make sure we clean up after ourselves
   GHC.defaultCleanupHandler dflags4 $ do

   when (showStartupMsg opts) $ liftIO $ startupMessage dflags4

   -- we've finished manipulating the DynFlags, update the session
   _ <- GHC.setSessionDynFlags dflags4
   dflags5 <- GHC.getSessionDynFlags

   ---------------- File parsing ---------------
   -- To simplify the handling of filepaths, we normalise all filepaths right
   -- away - e.g., for win32 platforms, backslashes are converted
   -- into forward slashes.
   let file_args    = if allowFileInput opts
                        then file_args1 ++ file_args2
                        else []
       file_paths   = map (normalise . GHC.unLoc) file_args
       (srcs, _)    = partitionFiles file_paths [] []

   ---------------- Display configuration ---------------
   when (verbosity dflags5 >= 4) $ liftIO $ dumpPackages dflags5
   when (verbosity dflags5 >= 3) $
      liftIO $ hPutStrLn stderr ("Hsc static flags: " ++ unwords staticFlags)

   ---------------- Do the business ---------------
   let safeCmds = case allowedGhciCommands opts of
          Nothing -> ghciCommands
          Just xs -> filter (\(x, _, _) -> x `elem` xs) ghciCommands
       ghciConfig = defaultGhciSettings {
             availableCommands = safeCmds,
             shortHelpText = fromMaybe (shortHelpText defaultGhciSettings)
                                       (shortHelp opts),
             fullHelpText  = fromMaybe (fullHelpText defaultGhciSettings)
                                       (longHelp opts)
          }

   handleSourceError (\e -> do
      GHC.printException e
      liftIO $ exitWith (ExitFailure 1)) $ ghciSafe ghciConfig srcs

ghciSafe :: GhciSettings -> [(FilePath, Maybe Phase)] -> GHC.Ghc ()
ghciSafe config srcs = do
   ghci_state <- liftIO $ defaultGHCiState config Nothing
   launchGHCi True (loadNoIO >> runGHCi srcs Nothing) ghci_state
   return ()

-- | GHCi startup message.
startupMessage :: GHC.DynFlags -> IO ()
startupMessage dflags = do
   let verb = verbosity dflags
   -- Show the GHCi banner
   when (verb >= 1) $ putStrLn ghciWelcomeMsg
   -- Display details of the configuration in verbose mode
   when (verb >= 2) $ do
      hPutStr stderr "Glasgow Haskell Compiler, Version "
      hPutStr stderr cProjectVersion
      hPutStr stderr ", stage "
      hPutStr stderr cStage
      hPutStr stderr " booted by GHC version "
      hPutStrLn stderr cBooterVersion

-----------------------------------------------------------------------------
-- GHC's flag parsing

-- | getTopDir parses the command line flags for the '-B <top dir>' argument.
getTopDir :: [String] -> (Maybe String, [String])
getTopDir args = (bVal, args')
   where
      (bArg, args') = partition ("-B" `isPrefixOf`) args
      bVal | null bArg = Nothing
           | otherwise = Just (drop 2 (last bArg))


type LStrings = [GHC.Located String]

-- | parseCmdLine parses the static flags and returns the dynamic flags
-- specified on the command line
parseCmdLine :: GHCiOptions
         -> IO (Maybe String, LStrings, LStrings)
parseCmdLine opts = do
   (topDir', args) <- getTopDir `fmap` getArgs
   let topDir = if parseTopDir opts && isJust topDir'
                  then topDir'
                  else defaultTopDir opts
       lArgs = map (GHC.mkGeneralLocated "on the commandline") args

   -- parse the static flags
   (flags, warns) <- GHC.parseStaticFlags lArgs

   return (topDir, flags, warns)


-- | partitionFiles splits arguments into source files and object files.
--
-- This is also where we interpret the -x <suffix> option (say what stage to
-- compile files of a certain suffix with). We attach a 'Maybe Phase' to each
-- source file to indicate the phase specified by '-x'.
partitionFiles :: [String] -> [(String, Maybe Phase)] -> [String]
               -> ([(String, Maybe Phase)], [String])
partitionFiles [] srcs objs = (reverse srcs, reverse objs)
partitionFiles ("-x":suff:args) srcs objs
   | "none" <- suff  = partitionFiles args srcs objs
   | StopLn <- phase = partitionFiles args srcs (slurp ++ objs)
   | otherwise       = partitionFiles rest (these_srcs ++ srcs) objs
         where phase        = startPhase suff
               (slurp,rest) = break (== "-x") args
               these_srcs   = zip slurp (repeat (Just phase))
partitionFiles (arg:args) srcs objs
   | isSourceFile arg = partitionFiles args ((arg,Nothing):srcs) objs
   | otherwise        = partitionFiles args srcs (arg:objs)

{-
We split out the object files (.o, .dll) and add them
to v_Ld_inputs for use by the linker.

The following things should be considered compilation manager inputs:

 - haskell source files (strings ending in .hs, .lhs or other
   haskellish extension),

 - module names (not forgetting hierarchical module names),

 - things beginning with '-' are flags that were not recognised by
   the flag parser, and we want them to generate errors later in
   checkOptions, so we class them as source files (#5921)

 - and finally we consider everything not containing a '.' to be
   a comp manager input, as shorthand for a .hs or .lhs filename.

Everything else is considered to be a linker object, and passed
straight through to the linker.
-}

-- | isSourceFile returns true if a file looks like a source file.
isSourceFile :: String -> Bool
isSourceFile m =  isSourceFilename m
               || looksLikeModuleName m
               || "-" `isPrefixOf` m
               || '.' `notElem` m

-----------------------------------------------------------------------------
-- GHCi Monad setup

loadNoIO :: GHCi ()
loadNoIO = do
    addImportToContext "import GHC.GHCi"
    addImportToContext "import Prelude"
    setRunMonad "NoIO"

-- :runmonad
-- Set the monad GHCi should execute in
setRunMonad :: String -> GHCi ()
setRunMonad name = GHC.setGHCiMonad name

