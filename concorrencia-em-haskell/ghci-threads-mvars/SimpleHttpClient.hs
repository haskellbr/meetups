module SimpleHttpClient
  where

import Control.Lens
import Data.ByteString.Lazy.Char8 (unpack)
import Network.Wreq

simpleGet :: String -> IO String
simpleGet url = do
    res <- get url
    return (unpack (res ^. responseBody))
