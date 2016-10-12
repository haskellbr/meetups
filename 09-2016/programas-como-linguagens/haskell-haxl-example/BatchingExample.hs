{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
module Main where

import           Control.Concurrent.Async
import           Control.Lens
import qualified Data.ByteString.Lazy     as ByteStringL
import           Data.Hashable
import           Data.List
import           Data.Monoid
import           Data.Text                (Text)
import qualified Data.Text                as Text
import qualified Data.Text.IO             as Text
import           Data.Typeable
import           Database.SQLite.Simple
import           Haxl.Core

type Haxl = GenHaxl ()

data Invoice = Invoice { invoiceId                :: Int
                       , invoiceCustomerId        :: Int
                       , invoiceDate              :: Text
                       , invoiceBillingAddr       :: Maybe Text
                       , invoiceBillingCity       :: Maybe Text
                       , invoiceBillingState      :: Maybe Text
                       , invoiceBillingCountry    :: Maybe Text
                       , invoiceBillingPostalCode :: Maybe Text
                       , invoiceBillingTotal      :: Double
                       }
  deriving(Show, Read)

instance FromRow Invoice where
    fromRow = Invoice
              <$> field
              <*> field
              <*> field
              <*> field
              <*> field
              <*> field
              <*> field
              <*> field
              <*> field

data SqlQuery r where
    GetInvoice :: Int -> SqlQuery Invoice
  deriving (Typeable)

deriving instance Eq (SqlQuery a)
deriving instance Show (SqlQuery a)
instance Show1 SqlQuery where show1 = show

instance Hashable (SqlQuery a) where
    hashWithSalt s (GetInvoice u) =
        hashWithSalt s u

instance StateKey SqlQuery where
    data State SqlQuery = SqlState { ssConn :: Connection
                                   }

instance DataSourceName SqlQuery where
    dataSourceName _ = "SqlQueryDataSource"

instance DataSource a SqlQuery where
    fetch SqlState{..} _flags _userEnv blockedFetches = AsyncFetch $ \idle -> do
        p <- runReqs blockedFetches
        idle
        wait p
      where
        runReqs :: [BlockedFetch SqlQuery] -> IO (Async ())
        runReqs bs = async $ do
            let ids :: [Int]
                vars :: [ResultVar Invoice]
                (ids, vars) = unzip
                  [ (invoiceId, resultVar) | (BlockedFetch r@(GetInvoice invoiceId) resultVar) <- bs ]
                sql :: String
                sql = unwords $ [ "SELECT * FROM Invoices WHERE InvoiceId IN ("
                                , (intercalate "," (map show ids))
                                ,  ")"
                                ]

            putStrLn ("querying " ++ show ids ++ "...")
            invoices <- query_ ssConn (Query (Text.pack sql)) :: IO [Invoice]
            print invoices
            putStrLn ("fetched " ++ show ids)

            mapM_ (uncurry putSuccess) (zip vars invoices)

batchExample :: Haxl [Invoice]
batchExample = do
    (r1, r2, r3, r4) <- (,,,) <$>
      dataFetch (GetInvoice 11) <*>
      dataFetch (GetInvoice 9) <*>
      dataFetch (GetInvoice 5) <*>
      dataFetch (GetInvoice 222)
    return [r1, r2, r3, r4]

main :: IO ()
main = do
    conn <- open "./chinook.db"
    setTrace conn (Just (\q -> Text.putStrLn q))
    let stateStore = stateSet SqlState { ssConn = conn
                                       } stateEmpty
    env0 <- initEnv stateStore ()
    [r1, r2, r3, r4] <- runHaxl env0 batchExample
    putStrLn $ "  r1 -> " <> show r1
    putStrLn $ "  r2 -> " <> show r2
    putStrLn $ "  r3 -> " <> show r3
    putStrLn $ "  r4 -> " <> show r4
