module Main where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import           System.IO
import           System.Process
-- import           Control.Monad.Trans.Resource
-- import           Data.Conduit.Process

data GhciOutput = StdOutput String
                | StdError String

readerThread :: TChan GhciOutput -> (Handle, Handle) -> IO ()
readerThread ghciChan (out, err) = do
    _ <- forkIO (readHandleLine StdOutput out)
    _ <- forkIO (readHandleLine StdError out)
    threadDelay (1000 * 1000 * 1000 * 1000)
  where
    readHandleLine t hnd = do
        ln <- hGetLine hnd
        atomically (writeTChan ghciChan (t ln))

writerThread :: Monad m => Handle -> m ()
writerThread inp  = return ()

main :: IO ()
main = do
    (Just inp, Just out, Just err, ph) <- createProcess
        (shell "ghci") { std_in = CreatePipe
                       , std_out = CreatePipe
                       , std_err = CreatePipe
                       }
    forkIO (writerThread inp)
    ghciChan <- newTChanIO
    forkIO (readerThread ghciChan (out, err))
    print inp
    print out
    print err
    serverThread

serverThread = forever serverThread
