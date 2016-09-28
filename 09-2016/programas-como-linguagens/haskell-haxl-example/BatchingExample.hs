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
import           Haxl.Prelude (forM)

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

data Customer = Customer { customerId :: Int
                         , customerFirstName :: Text
                         , customerLastName :: Text
                         , customerCompany :: Maybe Text
                         , customerAddr :: Maybe Text
                         , customerCity :: Maybe Text
                         , customerState :: Maybe Text
                         , customerCountry :: Maybe Text
                         , customerPostalCode :: Maybe Text
                         , customerPhone :: Maybe Text
                         , customerFax :: Maybe Text
                         , customerEmail :: Text
                         , customerSupportRepId :: Int
                         }
  deriving(Show, Read)

instance FromRow Customer where
    fromRow = Customer
              <$> field
              <*> field
              <*> field
              <*> field
              <*> field
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
    GetCustomer :: Int -> SqlQuery Customer
  deriving (Typeable)

deriving instance Eq (SqlQuery a)
deriving instance Show (SqlQuery a)
instance Show1 SqlQuery where show1 = show

instance Hashable (SqlQuery a) where
    hashWithSalt s (GetInvoice u) =
        hashWithSalt s [u, 0]
    hashWithSalt s (GetCustomer u) =
        hashWithSalt s [u, 1]

instance StateKey SqlQuery where
    data State SqlQuery = SqlState { ssConn :: Connection
                                   }

instance DataSourceName SqlQuery where
    dataSourceName _ = "SqlQueryDataSource"

instance DataSource a SqlQuery where
    fetch SqlState{..} _flags _userEnv blockedFetches = AsyncFetch $ \idle -> do
        p1 <- runInvoiceReqs blockedFetches
        p2 <- runCustomerReqs blockedFetches
        idle
        mapM_ wait [p1, p2]
      where
        runCustomerReqs :: [BlockedFetch SqlQuery] -> IO (Async ())
        runCustomerReqs bs = async $ do
            let customerIds :: [Int]
                vars :: [ResultVar Customer]
                (customerIds, vars) = unzip
                  [ (customerId, resultVar) | (BlockedFetch r@(GetCustomer customerId) resultVar) <- bs ]
                sql :: String
                sql = unwords $ [ "SELECT * FROM Customers WHERE CustomerId IN ("
                                , (intercalate "," (map show customerIds))
                                ,  ")"
                                ]
            case customerIds of
                [] -> return ()
                _ -> do
                    putStrLn ("querying " ++ show sql ++ "...")
                    invoices <- query_ ssConn (Query (Text.pack sql)) :: IO [Customer]
                    putStrLn ("fetched " ++ show sql)
                    mapM_ (uncurry putSuccess) (zip vars invoices)

        runInvoiceReqs :: [BlockedFetch SqlQuery] -> IO (Async ())
        runInvoiceReqs bs = async $ do
            let invoiceIds :: [Int]
                vars :: [ResultVar Invoice]
                (invoiceIds, vars) = unzip
                  [ (invoiceId, resultVar) | (BlockedFetch r@(GetInvoice invoiceId) resultVar) <- bs ]
                sql :: String
                sql = unwords $ [ "SELECT * FROM Invoices WHERE InvoiceId IN ("
                                , (intercalate "," (map show invoiceIds))
                                ,  ")"
                                ]

            case invoiceIds of
                [] -> return ()
                _ -> do
                  putStrLn ("querying " ++ show sql ++ "...")
                  invoices <- query_ ssConn (Query (Text.pack sql)) :: IO [Invoice]
                  putStrLn ("fetched " ++ show sql)
                  mapM_ (uncurry putSuccess) (zip vars invoices)

getInvoice i = dataFetch (GetInvoice i)
getCustomer i = dataFetch (GetCustomer i)

batchExample :: Haxl ([Invoice], [Customer])
batchExample = do
    invoices <- forM [11, 9, 5, 222] $ \invoiceId ->
        getInvoice invoiceId
    customers <- forM invoices $ \Invoice{..} ->
        getCustomer invoiceCustomerId
    return (invoices, customers)

main :: IO ()
main = do
    conn <- open "./chinook.db"
    -- setTrace conn (Just (\q -> Text.putStrLn q))
    let stateStore = stateSet SqlState { ssConn = conn
                                       } stateEmpty
    env0 <- initEnv stateStore ()
    ([i1, i2, i3, i4], [c1, c2, c3, c4]) <- runHaxl env0 batchExample
    putStrLn $ "  i1 -> " <> show i1
    putStrLn $ "  i2 -> " <> show i2
    putStrLn $ "  i3 -> " <> show i3
    putStrLn $ "  i4 -> " <> show i4
