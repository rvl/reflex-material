module Util.DevServer where

import Language.Javascript.JSaddle            (JSM)
import Language.Javascript.JSaddle.Run        (syncPoint)
import Language.Javascript.JSaddle.WebSockets (debugWrapper, jsaddleWithAppOr)
import Network.Wai                            (Application)
import Network.Wai.Handler.Warp               (defaultSettings, run, runSettings, setPort, setTimeout)
import Network.WebSockets                     (defaultConnectionOptions)
import Data.Function ((&))
import Data.Monoid ((<>))
import Network.Wai.Application.Static
import WaiAppStatic.Types

-- | A @main@ for doing development.
devServerMain :: Application -> JSM () -> Int -> IO ()
devServerMain backend frontend port = do
  putStrLn $ "Running dev server on localhost:" <> show port

  app <- jsaddleWithAppOr
    defaultConnectionOptions
    (frontend >> syncPoint)
    backend

  runSettings (defaultSettings & setTimeout 3600 & setPort port) app

-- | A version of @devServerMain@ that can be used with @ghcid --test@ to get an auto-reloading server.
devServerMainAutoReload :: Application -> JSM () -> Int -> IO ()
devServerMainAutoReload backend frontend port =
  debugWrapper $ \refreshMiddleware registerContext ->
    devServerMain (refreshMiddleware backend) (registerContext >> frontend) port

-- | Static file server with caching disabled.
staticServer :: String -> Application
staticServer path = staticApp ((defaultFileServerSettings path) & noCache)
  where noCache s = s { ssMaxAge = MaxAgeSeconds 0 }

staticServer2 :: Application
staticServer2 = staticApp ((defaultFileServerSettings "static") & noCache & fallThrough)
  where
    noCache s = s { ssMaxAge = MaxAgeSeconds 0 }
    fallThrough s = s { ss404Handler = Just vendorApp }
    vendorApp = staticApp ((defaultFileServerSettings "vendor") & noCache)
