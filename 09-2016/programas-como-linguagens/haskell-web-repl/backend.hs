{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
import           Control.Concurrent.Async
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.TH
import           Data.ByteString    (ByteString)
import qualified Data.ByteString.Lazy     as BL
import qualified Data.ByteString          as B hiding (unpack)
import qualified Data.ByteString.Char8    as B (unpack)
import           Data.Char
import           Data.Conduit
import qualified Data.Conduit.Binary as Conduit.Binary
import           Data.Conduit.Process
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text          as T
import qualified Data.Text.IO       as T
import           Data.Text.Encoding
import qualified Network.WebSockets as WS

data Message = Message { msgType :: T.Text
                       , msgPayload :: T.Text
                       }
  deriving(Show)

deriveJSON (defaultOptions { fieldLabelModifier = drop 3 . map toLower
                           }) ''Message

handler :: WS.Connection -> IO ()
handler conn = do
    let msgProducer = forever $ do
            mmsg <- decode <$> liftIO (WS.receiveData conn :: IO BL.ByteString)
            case mmsg of
                Just (Message "RUN" payload) -> void $ liftIO $ do
                    T.writeFile "./tmp.hs" payload

                    WS.sendTextData conn $ encode $ object
                        [ "type" .= String "RUN_COMMAND"
                        , "payload" .= String "stack ghc ./tmp.hs"
                        ]

                    (Inherited, out, err, cph) <- streamingProcess (shell "stack ghc ./tmp.hs")
                    let buildResultsSink = await >>= \case
                          Nothing -> return ()
                          Just l -> do
                              liftIO $ WS.sendTextData conn $ encode $ object
                                  [ "type" .= String "RUN_COMPILE_LOG"
                                  , "payload" .= String (decodeUtf8 l)
                                  ]
                              buildResultsSink
                    runConcurrently $
                        Concurrently (out $$ (Conduit.Binary.lines =$ buildResultsSink)) *>
                        Concurrently (err $$ (Conduit.Binary.lines =$ buildResultsSink)) *>
                        Concurrently (waitForStreamingProcess cph)

                    liftIO $ WS.sendTextData conn $ encode $ object
                        [ "type" .= String "COMPILE_DONE" ]

                    WS.sendTextData conn $ encode $ object
                        [ "type" .= String "RUN_COMMAND"
                        , "payload" .= String "./tmp"
                        ]

                    (Inherited, out, err, cph) <- streamingProcess (shell "./tmp")
                    let runResultsSink = await >>= \case
                          Nothing -> return ()
                          Just l -> do
                              liftIO $ WS.sendTextData conn $ encode $ object
                                  [ "type" .= String "RUN_LOG"
                                  , "payload" .= String (decodeUtf8 l)
                                  ]
                              runResultsSink
                    runConcurrently $
                        Concurrently (out $$ (Conduit.Binary.lines =$ runResultsSink)) *>
                        Concurrently (err $$ (Conduit.Binary.lines =$ runResultsSink)) *>
                        Concurrently (waitForStreamingProcess cph)

                    liftIO $ WS.sendTextData conn $ encode $ object
                        [ "type" .= String "RUN_DONE" ]

                Just (Message "LOAD_FILE" payload) -> do
                    liftIO $ T.writeFile "./tmp.hs" payload
                    yield (":load ./tmp.hs" <> "\n")

                Just (Message "RUN_AUTO" payload) -> void $ liftIO $ do
                    T.writeFile "./tmp.hs" payload

                    WS.sendTextData conn $ encode $ object
                        [ "type" .= String "RUN_COMMAND"
                        , "payload" .= String "stack-run-auto ./tmp.hs --resolver lts-6.17"
                        ]

                    (Inherited, out, err, cph) <- streamingProcess (shell "stack-run-auto ./tmp.hs")
                    let buildResultsSink h = await >>= \case
                          Nothing -> return ()
                          Just l -> do
                              liftIO $ WS.sendTextData conn $ encode $ object
                                  [ "type" .= String "RUN_LOG"
                                  , "payload" .= String (decodeUtf8 l)
                                  , "handle" .= String h
                                  ]
                              buildResultsSink h
                    runConcurrently $
                        Concurrently (out $$ (Conduit.Binary.lines =$ buildResultsSink "stdout")) *>
                        Concurrently (err $$ (Conduit.Binary.lines =$ buildResultsSink "stderr")) *>
                        Concurrently (waitForStreamingProcess cph)

                    liftIO $ WS.sendTextData conn $ encode $ object
                        [ "type" .= String "RUN_DONE" ]


                Just (Message "EVAL" payload) -> do
                    liftIO $ putStrLn ("<- \"" <> T.unpack payload <> "\"")
                    yield (encodeUtf8 payload <> "\n")
                Nothing -> liftIO $ print mmsg
        msgConsumer = await >>= \case
            Just "> " -> do
                liftIO $ putStrLn ("-> \"<DONE>\"")
                liftIO $ WS.sendTextData conn ("{\"type\":\"EVAL_DONE\"}" :: ByteString)
                msgConsumer
            Just msg -> do
                let msg' = fromMaybe msg (B.stripPrefix "> " msg)
                liftIO $ putStrLn ("-> \"" <> B.unpack msg' <> "\"")
                liftIO $ WS.sendTextData conn $ encode $ object $
                    [ "payload" .= String (decodeUtf8 msg')
                    , "type" .= String "EVAL_LOG"
                    ]
                msgConsumer
            Nothing -> return ()

    (inp, out, err, cph) <- streamingProcess (shell "ghci")

    (yield ":set prompt \"> \\n\"\n") $$ inp

    void $ runConcurrently $
        Concurrently (msgProducer $$ inp) *>
        Concurrently (out $$ (Conduit.Binary.lines =$ msgConsumer)) *>
        Concurrently (err $$ (Conduit.Binary.lines =$ msgConsumer)) *>
        Concurrently (waitForStreamingProcess cph)

main :: IO ()
main = do
    putStrLn "Listening on 9160"
    WS.runServer "0.0.0.0" 9160 $ \pendingConn -> do
        conn <- WS.acceptRequest pendingConn
        WS.forkPingThread conn 30
        handler conn
