-- stack runghc --verbose --package aeson --package bytestring-0.10.8.1 --package websockets --package conduit-extra --package conduit
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
import           Control.Concurrent.Async
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.TH
import           Data.ByteString    (ByteString)
import qualified Data.ByteString          as B hiding (unpack)
import qualified Data.ByteString.Char8    as B (unpack)
import           Data.Conduit
import qualified Data.Conduit.Binary as Conduit.Binary
import           Data.Conduit.Process
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text          as T
import qualified Network.WebSockets as WS

data Message = Message { msgType :: Text
                       , msgPayload :: Text
                       }

deriveJSON (defaultSettings { fieldLabelModifier = 2 }) ''Message

handler :: WS.Connection -> IO ()
handler conn = do
    let msgProducer = forever $ do
            msg <- decode <$> liftIO (WS.receiveData conn :: IO ByteString)
            liftIO $ putStrLn ("<- \"" <> B.unpack msg <> "\"")
            yield (msg <> "\n")
        msgConsumer = await >>= \case
            Just "> " -> do
                liftIO $ putStrLn ("-> \"<DONE>\"")
                liftIO $ WS.sendTextData conn ("<DONE>" :: ByteString)
                msgConsumer
            Just msg -> do
                let msg' = fromMaybe msg (B.stripPrefix "> " msg)
                liftIO $ putStrLn ("-> \"" <> B.unpack msg' <> "\"")
                liftIO $ WS.sendTextData conn msg'
                msgConsumer
            Nothing -> return ()

    (inp, out, err, cph) <- streamingProcess (shell "ghci")

    (yield ":set prompt \"> \\n\"\n") $$ inp

    runConcurrently $
        Concurrently (msgProducer $$ inp) *>
        Concurrently (out $$ (Conduit.Binary.lines =$ msgConsumer)) *>
        Concurrently (err $$ (Conduit.Binary.lines =$ msgConsumer)) *>
        Concurrently (waitForStreamingProcess cph)



    void $ sourceCmdWithStreams "ghci"
        msgProducer
        msgConsumer
        msgConsumer

main :: IO ()
main = do
    putStrLn "Listening on 9160"
    WS.runServer "0.0.0.0" 9160 $ \pendingConn -> do
        conn <- WS.acceptRequest pendingConn
        WS.forkPingThread conn 30
        handler conn
