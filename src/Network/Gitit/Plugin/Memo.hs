module Network.Gitit.Plugin.Memo (stdPlugin, memoPlugin) where
import Network.Gitit.Interface
import Text.Pandoc.Walk
import Text.Pandoc
import Data.Text (unpack)
import System.Random
import Util (split)
import Control.Monad ((>=>))
import Control.Exception
import Data.FileStore (FileStoreError(Unchanged), save, Author(Author), index, create)
import System.FilePath

{- Architecture of the Memo system

When reading a page:
- Scan page for certain "codes", which denote items for the memo sytem.
- Based on this info, create certain other pages, or update them if they already exist
- Then remove the "codes" from the page, leaving something "readable".
This is all handles by `stdPlugin`


To review pages:
- Memo pages shoulde contain metadata pertaining to the review process
- Still need to write the code for interacting with this.
- Memo pages also contain tags which are processed when loaded to determine what to show - for instance, clozes
This is handled by `memoPlugin`

Some remarks:
the 'memo-type' metadata is for setting the type of memo pages

functions "xxxPage" are for preprocessing ordinary pages,
while "xxxMemo" are for preprocessing memo pages, in general.

Clozes are built as [<text>](!cloze <tag>) - tag is optional.
When loading memo page, a random tag is chosen and all those clozes are hidden.
Untagged clozes are always hidden.
Tags *must* be enclosed in quotation marks.

Q/A pairs are built by divs with the "question" class. The first block
(usually the first paragraph) is the class. EG:

::: {.qapair}
Why is Foo Bar?

Baz Quux!
:::

-}
-------------
--- MISC/UTIL
-------------
-- Pick a random element from a list.
pick :: [a] -> PluginM (Maybe a) -- fails on infinite lists
pick [] = return Nothing
pick list = do
    idx <- liftIO $ randomRIO (0, length list - 1)
    return $ Just $ list !! idx


--- Make a page if it doesn't exist, or update it if it does.
makePage :: String -> String -> Pandoc -> PluginM ()
makePage filename header pandoc = do
    liftIO $ putStrLn $ "Making page " ++ filename
    filestore <- askFileStore
    idx <- liftIO $ index filestore
    let header' = "---\n"++ header ++ "\n...\n"
    pdoutput <- liftIO $ runIO $ writeMarkdown def pandoc
    liftIO $ putStrLn $ "Done w. markdown"
    case pdoutput of
        Left _ -> return () -- error - shouldn't happen.
        Right contents -> liftIO $ (if filename `elem` idx
            then catch (save filestore filename (Author "system" "")
                    "updated memo file" (header' ++ unpack contents)) (\Unchanged -> return ())
            else create filestore filename (Author "system" "") "Create memo file" 
                (header' ++ unpack contents))




------------------------------------------------------
--- Plugin for generating memo pages from normal pages
------------------------------------------------------

---Architectural issue: some of these kind of want both the raw text and the pandoc :/.
stdPlugin :: Plugin
stdPlugin = mkPageTransformM $ 
    clozePage >=> qaPage >=> listPage


-- This version of the cloze plugin are for creating clozed versions of whole pages
-- If you want smaller clozes, break up your pages.
clozePage :: Pandoc -> PluginM Pandoc
clozePage document = do
    md <- askMeta
    case lookup "cloze" md of
        Just "true" -> do
            doNotCache
            makeCloze document
            return $ walk unCloze document
        _ -> return document

-- Create a memo page if it doesn't exist. Just copy contents of this one
makeCloze :: Pandoc -> PluginM ()
makeCloze document = do
    file <- askFile
    let header = "memo-type: cloze"
    let filename = dropExtension file ++ ".memo.page"
    makePage filename header document

-- Remove cloze tags
unCloze :: Inline -> Inline
unCloze link@(Link attr ref (tag, _)) = case tag of
    "!cloze" -> Span attr ref
    _ -> link
unCloze x = x

qaPage :: Pandoc -> PluginM Pandoc
qaPage = walkM makeQA

makeQA :: Block -> PluginM Block
makeQA (Div (title, classes, keyvals) (q:as)) 
    | "qapair" `elem` classes = do
            file <- askFile
            let newDoc = Pandoc mempty [Div (title, "question":classes, keyvals) [q]
                                      , HorizontalRule
                                      , Div ("Answer", "answer":classes, keyvals) as]
            let filename = dropExtension file ++ "q" ++ title ++ ".memo.page"
            let header = "memo-type: question"
            makePage filename header newDoc
            return Null
makeQA x = return x


listPage :: Pandoc -> PluginM Pandoc
listPage = return -- unimplemented



------------------------------------------------------------
--- Plugin for transforming memo pages into reviewable form.
------------------------------------------------------------
--- TODO: Figure out a good "show answer" system.
--- Pressing a link is a bit slow.


memoPlugin :: Plugin
memoPlugin = mkPageTransformM $ \document -> do
    md <- askMeta
    case lookup "memo-type" md of
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

hideCloze :: (Maybe String) -> Inline -> Inline
hideCloze (Just tag) (Link attr conts ("!cloze", tags)) = 
    if tag `elem` Util.split ' ' tags || tags == ""
        then Str "____"
        else Span attr conts
hideCloze Nothing (Link attr conts ("!cloze", tags)) =
    if tags == ""
        then Str "____"
        else Span attr conts
hideCloze _ x = x

