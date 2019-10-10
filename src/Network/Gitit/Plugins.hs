{-# LANGUAGE CPP #-}
{-
Copyright (C) 2009 John MacFarlane <jgm@berkeley.edu>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

{- Functions for loading plugins.
-}

module Network.Gitit.Plugins ( loadPlugin, loadPlugins, compiledPlugins)
where
import Network.Gitit.Types
import System.FilePath (takeBaseName)
import Control.Monad (unless)
import System.Log.Logger (logM, Priority(..))
#ifdef _PLUGINS
import Data.List (isInfixOf, isPrefixOf)
import GHC
import GHC.Paths
import Unsafe.Coerce

-- Compiled plugins:
import qualified Network.Gitit.Plugin.Interwiki as Interwiki
import qualified Network.Gitit.Plugin.TikzCD as TikzCD
import qualified Network.Gitit.Plugin.MemomanNormal as Memoman
import qualified Network.Gitit.Plugin.DeadlinkDetection as DeadlinkDetection
import qualified Network.Gitit.Plugin.MemoCreate as MemoCreate
import qualified Network.Gitit.Plugin.TestSavePlugin as TestSavePlugin
import qualified Network.Gitit.Plugin.Backlinks as Backlinks

loadPlugin :: FilePath -> IO Plugin
loadPlugin pluginName = do
  logM "gitit" WARNING ("Loading plugin '" ++ pluginName ++ "'...")
  runGhc (Just libdir) $ do
    dflags <- getSessionDynFlags
    setSessionDynFlags dflags
    defaultCleanupHandler dflags $ do
      -- initDynFlags
      unless ("Network.Gitit.Plugin." `isPrefixOf` pluginName)
        $ do
            addTarget =<< guessTarget pluginName Nothing
            r <- load LoadAllTargets
            case r of
              Failed -> error $ "Error loading plugin: " ++ pluginName
              Succeeded -> return ()
      let modName =
            if "Network.Gitit.Plugin" `isPrefixOf` pluginName
               then pluginName
               else if "Network/Gitit/Plugin/" `isInfixOf` pluginName
                       then "Network.Gitit.Plugin." ++ takeBaseName pluginName
                       else takeBaseName pluginName
      pr <- parseImportDecl "import Prelude"
      i <- parseImportDecl "import Network.Gitit.Interface"
      m <- parseImportDecl ("import " ++ modName)
      setContext [IIDecl m, IIDecl  i, IIDecl pr]
      value <- compileExpr (modName ++ ".plugin :: Plugin")
      let value' = (unsafeCoerce value) :: Plugin
      return value'

#else

loadPlugin :: FilePath -> IO Plugin
loadPlugin pluginName = do
  error $ "Cannot load plugin '" ++ pluginName ++
          "'. gitit was not compiled with plugin support."
  return undefined

#endif

loadPlugins :: [FilePath] -> IO [Plugin]
loadPlugins pluginNames = do
  plugins' <- mapM loadPlugin pluginNames
  unless (null pluginNames) $ logM "gitit" WARNING "Finished loading plugins."
  return plugins'

compiledPlugins :: [Plugin]
compiledPlugins =
    [ TikzCD.plugin
    , DeadlinkDetection.plugin
    , Interwiki.plugin
    , TestSavePlugin.plugin
    , MemoCreate.plugin
    , Backlinks.plugin
    ]
  