module Network.Gitit.Plugin.MemomanReview (plugin) where

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
    case lookup "memo-mode" md of
        Just "cloze" -> doNotCache >> prepareCloze document
        _ -> return document


prepareCloze :: Pandoc -> PluginM Pandoc
prepareCloze document = do
    let tags = query getClozeTags document
    liftIO $ print tags
    tag <- pick tags
    return $ walk (hideCloze tag) document

getClozeTags :: Inline -> [String]
getClozeTags (Link _ _ ("!cloze", target)) = filter (/="") $ Util.split ' ' target
getClozeTags _ = []

hideCloze :: String -> Inline -> Inline
hideCloze tag (Link attr conts ("!cloze", tags)) = 
    if tag `elem` Util.split ' ' tags || tags == ""
        then Str "____"
        else Span attr conts

hideCloze _ x = x