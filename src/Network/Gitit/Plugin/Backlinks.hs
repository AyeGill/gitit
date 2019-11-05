module Network.Gitit.Plugin.Backlinks (plugin) where

import Control.Exception
import Data.FileStore (FileStore, FileStoreError(Unchanged), retrieve, save, Author(Author), index, create)

import Network.Gitit.ContentTransformer (inlinesToString)
import Network.Gitit.Interface
import Network.Gitit.Framework ()

import Data.List (isPrefixOf)
import Data.List.Utils (uniq, replace)

import qualified Data.Map as Map
import Data.Map (Map)

import Data.Aeson
import Text.Pandoc.Walk (query)

blfile :: FilePath
blfile = "backlinks.json"
-- Maps each page to the set of pages it links TO
-- (Could also have been page -> backlinks)

plugin :: Plugin
plugin = mkPageTransformM doBacklinks

doBacklinks :: Pandoc -> PluginM Pandoc
doBacklinks document = do
    askFileStore >>= (liftIO . ensureBl)
    updateBacklinks document
    addBacklinks document

-- If internal wiki link, extract linked page
-- Use same hack as DeadlinkDetection.hs
findWiki :: Inline -> Maybe String
findWiki (Link _ inlines (_, "Go to wiki page")) = Just $ inlinesToString inlines
findWiki (Link _ _ (url, _)) 
    | not("http" `isPrefixOf` url || "!" `isPrefixOf` url) = Just $ replace "%20" " " url
-- Try to avoid flagging (1): External links, (2): Commands used by something else.
-- Also replace the "formatted" spaces in the url
findWiki _ = Nothing

-- Find pages linked to in document
extractInternalLinks :: Pandoc -> [String]
extractInternalLinks document = uniq $ query go document
    where go :: Inline -> [String]
          go inl = case findWiki inl of
                Nothing -> []
                Just x -> [x]

updateBacklinks :: Pandoc -> PluginM ()
updateBacklinks document = do
    liftIO $ putStrLn "Updating backlinks..."
    let links = extractInternalLinks document
    layout <- askLayout
    let title = pgPageName layout
    saveBacklinks title links

-- Read backlinks from file and add them to the page
addBacklinks :: Pandoc -> PluginM Pandoc
addBacklinks (Pandoc meta blocks) = do
    layout <- askLayout
    let title = pgPageName layout
    liftIO $ putStrLn "adding backlinks"
    backlinks <- readBacklinks title
    return $ Pandoc meta $ blocks ++ [Header 2 nullAttr [Str "Backlinks"], formatBacklinks backlinks]
    
formatBacklinks :: [String] -> Block
formatBacklinks links = BulletList $ map (\page -> [Plain [Link nullAttr [Str page] (page, "")]]) links


-- Save links to the backlinks directory
saveBacklinks :: String -> [String] -> PluginM ()
saveBacklinks title links = do
    filestore <- askFileStore
    user <- askUser
    liftIO $ putStrLn "Loading blfile to update it.."
    raw <- liftIO $ retrieve filestore blfile Nothing
    let bd = case decode raw of
            Nothing -> Map.empty
            Just dat -> dat
    liftIO $ putStrLn "Writing blfile.."
    liftIO $ catch (save filestore blfile (maybe (Author "nobody" "") (\(User n _ e) -> Author n e) user) 
        "updated backlinks" $ encode $ Map.insert title links bd) (\Unchanged -> return ())
    -- Write the blfile. If it was unchanged, just proceed. Use user as auth.

ensureBl :: FileStore -> IO ()--touch file if missing
ensureBl filestore = do
    idx <- index filestore
    if blfile `elem` idx
        then putStrLn "blfile found"
        else create filestore blfile (Author "system" "") "Initialize backlink file" 
            (encode (Map.empty :: Map String [String]))

-- Find backlinks for current page from file
readBacklinks :: String -> PluginM [String]
readBacklinks title = do
    filestore <- askFileStore
    raw <- liftIO $ retrieve filestore blfile Nothing
    let linkList = case decode raw of
            Nothing -> [] :: [(String, [String])]
            Just dat -> Map.toList dat
    return $ map fst $ filter (\(_, links) -> title `elem` links) linkList
    