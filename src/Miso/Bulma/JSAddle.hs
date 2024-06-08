{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Miso.Bulma.JSAddle
  ( run
  , runWith
  , debug
  , debugWith

  , jsaddleAppWithBulma
  , BulmaJSAddleOptions(..)
  , defaultOptions
  ) where

import           Data.ByteString.Lazy (ByteString, fromStrict)
import           Data.FileEmbed (makeRelativeToProject, embedFile)
import           Language.Javascript.JSaddle.Run (syncPoint)
import           Language.Javascript.JSaddle.Types (JSM)
import           Language.Javascript.JSaddle.WebSockets (jsaddleJs, jsaddleOr, debugOr)
import           Network.HTTP.Types (status200,status403)
import qualified Network.Wai as Wai
import           Network.Wai.Handler.Warp (defaultSettings, setTimeout, setPort, runSettings)
import           Network.WebSockets (defaultConnectionOptions)
-- import           Paths_miso_bulma

--------------------------------------------------------------------------------

-- | Run the given 'JSM' action that somehow uses bulma using Warp server on the given
-- port on GHC.
run :: Int -> JSM () -> IO ()
run = runWith defaultOptions

-- | Run the application with the given bulma settings
runWith             :: BulmaJSAddleOptions -> Int -> JSM () -> IO ()
runWith opts port f =
    runSettings (setPort port (setTimeout 3600 defaultSettings)) =<<
        jsaddleOr defaultConnectionOptions (f >> syncPoint) (jsaddleAppWithBulma opts)

-- | Run a debugging instance to allow easy reloading; using the default BulmaJSOptions.
debug :: Int -> JSM () -> IO ()
debug = debugWith defaultOptions

-- | Run a JSM action that uses bulma in debuggin mode. Allows specifying bulmaJSAddle
-- options.
debugWith             :: BulmaJSAddleOptions -> Int -> JSM () -> IO ()
debugWith opts port f = debugOr port f (jsaddleAppWithBulma opts)

--------------------------------------------------------------------------------

-- | Application that serves the paths:
--
-- - '/'           : returns an index.html that is suitable for using jsaddle and bulma.
-- - '/jsaddle.js' : the default jsaddle.js script
jsaddleAppWithBulma :: BulmaJSAddleOptions -> Wai.Application
jsaddleAppWithBulma (BulmaJSAddleOptions {..}) req sendResponse =
    sendResponse $ case (Wai.requestMethod req, Wai.pathInfo req) of
      ("GET", [])             -> respond200 indexHtml
      ("GET", ["jsaddle.js"]) -> respond200 jsaddleJavaScript
      _                       -> forbidden
  where
    respond200  = Wai.responseLBS status200 [("Content-Type", "text/html")]
    forbidden   = Wai.responseLBS status403 [("Content-Type", "text/plain")] "Forbidden"

--------------------------------------------------------------------------------

-- | Options we can specify to run a miso-bulma application using jsaddle
data BulmaJSAddleOptions =
  BulmaJSAddleOptions { indexHtml         :: ByteString
                        -- ^ the content of the index.html  file
                      , jsaddleJavaScript :: ByteString
                        -- ^ the content of the jdsaddle.js file
                      } deriving (Show,Eq)

-- | Default options
defaultOptions :: BulmaJSAddleOptions
defaultOptions = BulmaJSAddleOptions
                 { indexHtml         = withBulmaCDNFile
                 , jsaddleJavaScript = jsaddleJs False
                 }

withBulmaCDNFile :: ByteString
withBulmaCDNFile = fromStrict $
                   $(makeRelativeToProject "resources/indexWithBulmaCDN.html" >>= embedFile)
