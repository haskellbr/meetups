{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
module Main where

import           Control.Concurrent.Async
import           Control.Lens
import qualified Data.ByteString.Lazy     as ByteStringL
import           Data.Hashable
import           Data.Monoid
import           Data.Typeable
import           Haxl.Core
import           Network.Wreq

type Haxl = GenHaxl ()

data HttpReq a where
    GetUrl :: String -> HttpReq ByteStringL.ByteString
  deriving (Typeable)

deriving instance Eq (HttpReq a)
deriving instance Show (HttpReq a)
instance Show1 HttpReq where show1 = show

instance Hashable (HttpReq a) where
    hashWithSalt s (GetUrl u) =
        hashWithSalt s u

instance StateKey HttpReq where
    data State HttpReq = HttpState {}

instance DataSourceName HttpReq where
    dataSourceName _ = "HttpReqDataSource"

instance DataSource a HttpReq where
    fetch _state _flags _userEnv blockedFetches = AsyncFetch $ \idle -> do
        ps <- mapM runReq blockedFetches
        idle
        mapM_ wait ps
      where
        runReq :: BlockedFetch HttpReq -> IO (Async ())
        runReq (BlockedFetch (GetUrl url) resultVar) = async $ do
            putStrLn ("fetching " ++ url ++ "...")
            res <- get url
            putStrLn ("fetched " ++ url)
            putSuccess resultVar (view responseBody res :: ByteStringL.ByteString)

cacheExample :: Haxl [ByteStringL.ByteString]
cacheExample = do
    (r1, r2, r3, r4) <- (,,,) <$>
      dataFetch (GetUrl "https://stackoverflow.com") <*>
      dataFetch (GetUrl "https://stackoverflow.com") <*>
      dataFetch (GetUrl "https://stackoverflow.com") <*>
      dataFetch (GetUrl "https://beijaflor.io")
    return [r1, r2, r3, r4]

main :: IO ()
main = do
    let stateStore = stateSet HttpState{} stateEmpty
    env0 <- initEnv stateStore ()
    [r1, r2, r3, r4] <- runHaxl env0 cacheExample
    putStrLn $ "  r1 -> " <> show (ByteStringL.length r1)
    putStrLn $ "  r2 -> " <> show (ByteStringL.length r2)
    putStrLn $ "  r3 -> " <> show (ByteStringL.length r3)
    putStrLn $ "  r4 -> " <> show (ByteStringL.length r4)
