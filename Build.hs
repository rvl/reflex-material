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
  want ["ghcjs", "assets", "docs/out.js"]

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
    let dst = takeDirectory out
    getDirectoryFiles jsexe ["//*"] >>= mapM_ (\f -> do
      let dst' = dst </> f
      liftIO $ createDirectoryIfMissing True (takeDirectory dst')
      copyFile' (jsexe </> f) dst')

copyAssets :: FilePath -> Action ()
copyAssets dst = do
  assets <- getDirectoryFiles "" ["static//*"]
  need assets
  forM_ assets $ \f -> do
    let dst' = dst </> dropDirectory1 f
    liftIO $ createDirectoryIfMissing True (takeDirectory dst')
    copyFile' f dst'

copyNodeModules :: FilePath -> Action ()
copyNodeModules dst = do
  nodeModules <- getEnvWithDefault "node_modules" "NODE_MODULES"
  exists <- doesDirectoryExist nodeModules
  unless exists $ fail "can't find node_modules"
  contents <- getDirectoryFiles nodeModules ["//*.min.js", "//*.min.css"]
  forM_ contents $ \f -> do
    let dst' = dst </> "node_modules" </> f
    liftIO $ createDirectoryIfMissing True (takeDirectory dst')
    copyFile' (nodeModules </> f) dst'
