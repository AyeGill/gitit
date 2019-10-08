module Network.Gitit.Plugin.MemoCreate (plugin) where

{- 
Processes pages and formats them for memo review.
- Finds cloze tags formatted like so: [Text](!cloze), and inserts blanks.
- Tags like [Text](!cloze {tag}) result in a random tag being chosen, and
  only those clozes being removed (except untagged clozes, which are always removed).
  Multiple-tagged clozes are hidden if either of the tags are chosen
  (I don't know when this will be useful). Write [Text](!cloze tag1 tag2).
  This means tags can't have spaces.
-}

import Network.Gitit.Interface
import Text.Pandoc.Walk
import Text.Pandoc.Writers (writeMarkdown)
import System.Random
import Util (split)

pick :: [a] -> PluginM a -- fails on infinite lists
pick list = do
    idx <- liftIO $ randomRIO (0, length list - 1)
    liftIO $ print idx
    return $ list !! idx

plugin :: Plugin
plugin = mkPageTransformM $ \document -> do
    md <- askMeta
    case Util.split ',' <$> lookup "memo-styles" md of
        Nothing -> return document
        Just styles -> 
            mapM_ (run document) styles >> return document


run :: Pandoc -> String -> PluginM ()
run doc "cloze" = liftIO $ putStrLn "We would've written something to store"
run doc style = return ()