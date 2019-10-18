module Network.Gitit.Plugin.TikzCD (plugin) where
{-

This plugin provides a tikzcd output.
(* latex and dvipng executable must be in the path.)

like this:

~~~ {.tikzcd}
A \ar[r] & B \ar[d]\\
 & C
~~~

License: GPL
written by Kohei OZAKI <i@smly.org>
modified by John MacFarlane to use withTempDir
modified by Eigil Rischel for tikzcd
-}

import Network.Gitit.Interface
import System.Process (system)
import System.Directory
import Data.ByteString.Lazy.UTF8 (fromString)
import Data.Digest.Pure.SHA
import System.FilePath
import Control.Monad.Trans (liftIO)
import System.Log.Logger (logM, Priority(..))

plugin :: Plugin
plugin = mkPageTransformM transformBlock

templateHeader, templateFooter :: String
templateHeader = concat
    [ "\\documentclass[12pt]{article}\n"
      , "\\usepackage{amsmath,amssymb,bm,tikz-cd}\n"
      , "\\begin{document}\n"
      , "\\thispagestyle{empty}\n"
      , "\\begin{tikzcd}\n"]

templateFooter =
      "\n"
      ++ "\\end{tikzcd}\n"
      ++ "\\end{document}\n"

transformBlock :: Block -> PluginM Block
transformBlock (CodeBlock (_, classes, namevals) contents)
    | "tikzcd" `elem` classes = do
  liftIO $ logM "gitit" DEBUG"compiling .tex image..."
  cfg <- askConfig
  let (name, outfile) =  case lookup "name" namevals of
                                Just fn   -> ([Str fn], fn ++ ".svg")
                                Nothing   -> ([], uniqueName contents ++ ".svg")
  curr <- liftIO getCurrentDirectory
  liftIO $ withTempDir "gitit-imgtex" $ \tmpdir -> do
    setCurrentDirectory tmpdir
    writeFile (outfile ++ ".tex") (templateHeader ++ contents ++ templateFooter)
    system $ "latex " ++ outfile ++ ".tex -interaction=batchmode" --find a solution to display error if diagram is fucked.
    setCurrentDirectory curr
    system $ "dvisvgm -n -e " ++
        (tmpdir </> outfile <.> "dvi") ++ " -o " ++ (staticDir cfg </> "img" </> outfile) ++ " > /dev/null"
    return $ Para [Image nullAttr name ("/img" </> outfile, "")]
transformBlock x = return x

uniqueName :: String -> String
uniqueName = showDigest . sha1 . fromString
