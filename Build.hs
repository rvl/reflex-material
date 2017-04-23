import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
import Control.Monad (forM_, unless)
import System.Directory (createDirectoryIfMissing)

-- before running this, need to either
-- nix-build mdc.nix -A package
-- export NODE_MODULES=./result/lib/node_modules/reflex-material/node_modules
--   or
-- npm install

jsexe :: FilePath
jsexe = "dist/build/reflex-material-exe/reflex-material-exe.jsexe"

jsexeFiles :: [FilePath]
jsexeFiles = [jsexe </> f | f <- ["all.js", "lib.js", "out.js", "rts.js", "runmain.js"]]

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="dist"} $ do
  want ["ghcjs", "assets", "docs/out.js", "docs/.nojekyll"]

  phony "ghcjs" $ do
    need [jsexe </> "out.js", jsexe </> "index.html"]

  phony "assets" $ do
    copyAssets jsexe
    copyNodeModules jsexe

  phony "clean" $ do
    putNormal "Cleaning files in dist"
    removeFilesAfter "dist" ["//*"]

  "dist/setup-config" %> \out -> do
    need ["reflex-material.cabal"]
    cmd "cabal configure --ghcjs"

  jsexeFiles &%> \out -> do
    sources <- getDirectoryFiles "" ["src//*.hs", "example//*.hs"]
    need ("dist/setup-config" : sources)
    cmd "cabal build"

  jsexe </> "index.html" %> \out -> do
    orderOnly [takeDirectory out </> "out.js"]
    copyFile' "static/index.html" out

  "docs/out.js" %> \out -> do
    orderOnly [jsexe </> "out.js"]
    let dst = takeDirectory out
    getDirectoryFiles jsexe ["//*"] >>= mapM_ (\f -> do
      let dst' = dst </> f
      liftIO $ createDirectoryIfMissing True (takeDirectory dst')
      copyFile' (jsexe </> f) dst')

  -- github pages jekyll filters some things
  "docs/.nojekyll" %> \out -> writeFile' out ""

copyAssets :: FilePath -> Action ()
copyAssets dst = do
  assets <- getDirectoryFiles "" ["static//*"]
  need assets
  forM_ assets $ \f -> do
    let dst' = dst </> dropDirectory1 f
    need [f]
    liftIO $ createDirectoryIfMissing True (takeDirectory dst')
    copyFile' f dst'

libCss = [ "material-components-web/dist/material-components-web.min.css"
         , "material-design-icons/iconfont/material-icons.css"
         , "roboto-fontface/css/roboto/roboto-fontface.css"
         ]
libJs =  [ "material-components-web/dist/material-components-web.min.js" ]
libFonts = [ "roboto-fontface/fonts/Roboto"
           , "material-design-icons/iconfont" ]

copyLib :: FilePath -> [FilePath] -> Action ()
copyLib dst fs = mapM_ copy fs
  where copy f = copyFile' f (dst </> takeFileName f)

fontPats = ["//*.ttf", "//*.woff", "//*.woff2", "//*.eot", "//*.svg"]

copyFonts :: FilePath -> FilePath -> [FilePath] -> Action ()
copyFonts dst src fs = forM_ libFonts $ \font -> do
  fonts <- getDirectoryFiles (src </> font) fontPats
  forM_ fonts $ \f -> do
    copyFile' (src </> font </> f) (dst </> takeFileName f)

copyNodeModules :: FilePath -> Action ()
copyNodeModules dst = do
  nodeModules <- getNodeModules
  liftIO $ forM_ ["css", "js", "fonts"] $
    \d -> createDirectoryIfMissing True (dst </> d)
  copyLib (dst </> "css") $ map (nodeModules </>) libCss
  copyLib (dst </> "js") $ map (nodeModules </>) libJs
  copyFonts (dst </> "fonts") nodeModules libFonts

getNodeModules :: Action FilePath
getNodeModules = do
  nodeModules <- getEnvWithDefault "node_modules" "NODE_MODULES"
  exists <- doesDirectoryExist nodeModules
  unless exists $ fail "can't find node_modules"
  return nodeModules
