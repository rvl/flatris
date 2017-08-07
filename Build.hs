import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
import Control.Monad (forM_, unless)
import System.Directory (createDirectoryIfMissing)

jsexe :: FilePath
jsexe = "dist/build/flatris-reflex/flatris-reflex.jsexe"

scripts :: [FilePath]
scripts = ["all.js", "lib.js", "out.js", "rts.js", "runmain.js"]

jsexeFiles :: [FilePath]
jsexeFiles = map (jsexe </>) scripts

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="dist"} $ do
  want ["docs/index.html", "docs/.nojekyll"]

  phony "example" $ need ["cabalBuild", "assets", "docs/out.js"]

  phony "cabalBuild" $
    need [jsexe </> "out.js", jsexe </> "index.html"]

  phony "clean" $ do
    putNormal "Cleaning files in dist"
    removeFilesAfter "dist" ["//*"]

  "dist/setup-config" %> \out -> do
    need ["flatris.cabal"]
    cmd "cabal configure --ghcjs"

  jsexeFiles &%> \out -> do
    needHaskellSources
    cmd "cabal build"

  jsexe </> "*.min.js" %> \out -> do
    let maxi = dropExtension out -<.> "js"
        externs = maxi <.> "externs"
    need [maxi]
    Stdout mini <- cmd "closure-compiler" [maxi] "--compilation_level=ADVANCED_OPTIMIZATIONS" ["--externs=" ++ externs]
    writeFileChanged out mini

  jsexe </> "*.js.gz" %> \out -> do
    let js = dropExtension out
    need [js]
    cmd "zopfli -i1000" [js]

  -- site needs index.html, minified js, and stylesheets
  "docs/index.html" %> \out -> do
    forM_ ["all.min.js", "all.min.js.gz"] $ \js ->
      copyFileChanged (jsexe </> js) ("docs" </> js)
    copyAssets "docs"

  -- github pages jekyll filters some things
  "docs/.nojekyll" %> \out -> writeFile' out ""

needHaskellSources :: Action ()
needHaskellSources = do
  sources <- getDirectoryFiles "" ["src//*.hs", "app-reflex//*.hs"]
  need ("dist/setup-config" : sources)

copyAssets :: FilePath -> Action ()
copyAssets dst = do
  assets <- getDirectoryFiles "" ["static//*"]
  need assets
  forM_ assets $ \f -> do
    let dst' = dst </> dropDirectory1 f
    need [f]
    liftIO $ createDirectoryIfMissing True (takeDirectory dst')
    copyFile' f dst'
