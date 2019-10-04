module Network.Gitit.Plugin.MemomanNormal (plugin) where

{- 
Processes pages and removes memo-formatting.
- Finds cloze tags formatted like so: [Text](!cloze), and just inserts the text.
-}

import Network.Gitit.Interface

plugin :: Plugin
plugin = mkPageTransform unCloze

unCloze :: Inline -> Inline
unCloze link@(Link attr ref (tag, _)) = case tag of
    "!cloze" -> Span attr ref
    _ -> link
unCloze x = x



