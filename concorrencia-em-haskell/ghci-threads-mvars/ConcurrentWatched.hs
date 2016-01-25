{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module ConcurrentWatched
    (
      forkIO
    , allThreads
    , module Control.Concurrent
    )
  where

import           Control.Concurrent         hiding (forkIO)
import qualified Control.Concurrent         as Concurrent (forkIO)
import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as ByteString (putStrLn)
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import qualified Data.Text                  as Text (pack)
import           GHC.Generics
import           System.IO.Unsafe

data ThreadState = ThreadRunning
                 | ThreadStopped
  deriving(Generic)

instance ToJSON ThreadState where
    toJSON ThreadRunning = String "running"
    toJSON ThreadStopped = String "stopped"

threadStatesJSON :: IO ()
threadStatesJSON = do
    threads <- readMVar allThreads
    states <- forM (Map.toList threads) $ \(tid, threadFinished) -> do
        mf <- tryReadMVar threadFinished
        case mf of
            Nothing -> return (tid, ThreadRunning)
            Just _ -> return (tid, ThreadStopped)
    ByteString.putStrLn (encode $ object (map (\(k, v) -> Text.pack (show k) .= v) states))

{-# NOINLINE allThreads #-}
allThreads :: MVar (Map ThreadId (MVar ()))
allThreads = unsafePerformIO $
    newMVar Map.empty

forkIO :: IO () -> IO ThreadId
forkIO acao = do
    finished <- newEmptyMVar
    tid <- Concurrent.forkIO $ do
        acao
        putMVar finished ()

    modifyMVar_ allThreads $ \threads ->
        return (Map.insert tid finished threads)

    return tid
