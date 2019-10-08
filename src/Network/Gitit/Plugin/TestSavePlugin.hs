module Network.Gitit.Plugin.TestSavePlugin (plugin) where

import System.Directory (getHomeDirectory)
import Network.Gitit.Interface (liftIO, bottomUpM, PluginM, Plugin(PreCommitTransform), Inline(Link))
import Text.Pandoc (readMarkdown, def)
import Text.Pandoc.Class (runPure)
import Text.Pandoc.Walk (walkM)
import Data.Text as T

plugin = PreCommitTransform printLinks

printLinks :: String -> PluginM String
printLinks string = do
    let pandoc' = runPure $ readMarkdown def $ T.pack string
    case pandoc' of
        Left _ -> return string
        Right pandoc -> do 
            p' <- liftIO $ walkM printLink pandoc
            return string

printLink :: Inline -> IO Inline
printLink link@(Link _ _ (u, a)) = putStrLn u >> putStrLn a >> return link
printLink x = return x

