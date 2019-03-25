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
jsexe = ghcjsDist </> "build/reflex-material-example/reflex-material-example.jsexe"

haddocks :: FilePath
haddocks = ghcDist </> "doc/html/reflex-material"

scripts :: [FilePath]
scripts = ["all.js", "lib.js", "out.js", "rts.js", "runmain.js"]

jsexeFiles :: [FilePath]
jsexeFiles = map (jsexe </>) scripts

ghcjsDist :: FilePath
ghcjsDist = "dist-ghcjs"

ghcDist :: FilePath
ghcDist = "dist"

-- | A file that indicates cabal configure has been run.
configuredMarker :: FilePath -> FilePath
configuredMarker distDir = distDir </> "setup-config"

vendorFiles :: [FilePath]
vendorFiles = [ "vendor/css/material-components-web.min.css"
              , "vendor/js/material-components-web.min.js" ]

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles=ghcjsDist} $ do
  want ["example", "haddock", "docs/.nojekyll"]

  phony "example" $ need ["cabalBuild", "assets", "docs/all.js"]
  phony "vendorFiles" $ need vendorFiles

  phony "cabalBuild" $
    need [jsexe </> "all.js", jsexe </> "index.html"]

  phony "assets" $ do
    copyAssets jsexe
    copyNodeModules jsexe

  phony "assets_for_stack" $ do
    jsexeStack <- getEnv "JSEXE"
    case jsexeStack of
      Just jsexe -> do
        copyAssets jsexe
        copyNodeModules jsexe
      Nothing -> fail "Run this rule from the Makefile"

  phony "haddock" $ need ["docs/doc/index.html"]

  phony "clean" $ do
    putNormal $ "Cleaning files in " ++ ghcjsDist
    removeFilesAfter ghcjsDist ["//*"]

  configuredMarker ghcjsDist %> \out -> do
    need ["reflex-material.cabal"]
    cmd "nix-shell default.nix -A shells.ghcjs --run" ["cabal configure --ghcjs --builddir=" ++ ghcjsDist]

  jsexeFiles &%> \out -> do
    needHaskellSources
    need vendorFiles
    cmd "cabal build" ["--builddir=" ++ ghcjsDist]

  configuredMarker ghcDist %> \out -> do
    need ["reflex-material.cabal"]
    cmd "nix-shell default.nix -A shells.ghc --run" ["cabal configure --builddir=" ++ ghcDist]

  haddocks </> "index.html" %> \out -> do
    needHaskellSources
    need [configuredMarker ghcDist]
    cmd "nix-shell default.nix -A shells.ghc --run" ["cabal haddock --builddir=" ++ ghcDist]

  jsexe </> "index.html" %> \out -> do
    orderOnly [takeDirectory out </> "all.js"]
    copyFile' "static/index.html" out

  "docs/all.js" %> \out -> do
    orderOnly [jsexe </> "all.js"]
    let dst = takeDirectory out
    getDirectoryFiles jsexe ["//*"] >>= mapM_ (\f -> do
      let dst' = dst </> f
      liftIO $ createDirectoryIfMissing True (takeDirectory dst')
      copyFile' (jsexe </> f) dst')
    copyAssets "docs" -- fixme: reorganise build

  vendorFiles &%> \out -> do
    copyNodeModules "vendor"

  "docs/doc/index.html" %> \out -> do
    orderOnly [haddocks </> "index.html"]
    docs <- getDirectoryFiles "" [haddocks ++ "//*"]
    forM_ docs $ \doc -> copyFile' doc (takeDirectory out </> takeFileName doc)

  jsexe </> "*.min.js" %> \out -> do
    let maxi = dropExtension out -<.> "js"
        externs = maxi <.> "externs"
    need [maxi]
    Stdout mini <- cmd "closure-compiler" [maxi] "--compilation_level=ADVANCED_OPTIMIZATIONS" ["--externs=" ++ externs]
    writeFileChanged out mini

  jsexe </> "*.js.gz" %> \out -> do
    let js = [dropExtension out]
    need js
    cmd "zopfli -i1000" js

  -- github pages jekyll filters some things
  "docs/.nojekyll" %> \out -> writeFile' out ""

needHaskellSources :: Action ()
needHaskellSources = do
  sources <- getDirectoryFiles "" ["src//*.hs", "example//*.hs"]
  need (configuredMarker ghcjsDist : sources)

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
