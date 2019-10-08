module Network.Gitit.Plugin.DeadlinkDetection (plugin) where

-- code lifted from subst plugin
import Control.Exception
import Data.FileStore (FileStoreError, retrieve)

import Network.Gitit.ContentTransformer (inlinesToString)
import Network.Gitit.Interface
import Network.Gitit.Framework (filestoreFromConfig)

import Data.List (isPrefixOf)
import Data.List.Utils (split, join)

plugin :: Plugin
plugin = mkPageTransformM alterLink

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace old new = join new . split old

-- Stolen from missingH

-- Check if a link is an internal wiki link or not, call deadenLink if it is
alterLink :: Inline -> PluginM Inline
alterLink link@(Link _ _ (_, _)) = do
    let target = findWiki link
    case target of
        Just target' -> deadenLink target' link
        Nothing -> return link
alterLink x = return x

findWiki :: Inline -> Maybe String
findWiki (Link _ inlines (_, "Go to wiki page")) = Just $ inlinesToString inlines
findWiki (Link _ _ (url, _)) 
    | not("http" `isPrefixOf` url || "!" `isPrefixOf` url) = Just $ replace "%20" " " url
-- Try to avoid flagging (1): External links, (2): Commands used by something else.
-- Also replace the "formatted" spaces in the url
findWiki _ = Nothing



-- Given a link assumed to be an internal wiki link, deaden if necessary
deadenLink :: String -> Inline -> PluginM Inline
deadenLink target link@(Link (ident, classes, attrs) inlines targ) = do
    cfg <- askConfig
    let fs = filestoreFromConfig cfg
    liftIO $ putStrLn $ "Looking for" ++ target
    article <- liftIO $ try (retrieve fs (target ++ ".page") Nothing)
    case article :: Either FileStoreError String of
        Left _ -> return $ Link (ident, "deadlink":classes, attrs) inlines targ --Make the link red
        Right _ -> liftIO (putStrLn $ "Link live" ++ target) >> return link

deadenLink _ x = return x -- Should never happen
