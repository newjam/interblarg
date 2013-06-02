{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}


import Web.Scotty

import Data.Conduit
import qualified Data.Conduit.List as CL

import Data.Monoid (mconcat)
import Control.Monad.IO.Class (liftIO, MonadIO)
import qualified Data.ByteString as B

import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.ByteString

import qualified Database.Redis as R
import Database.Redis.PubSub
import Database.Redis.Core

import Network.Wai.EventSource


main = scotty 8000 $ do
  get "/chat.js" $ file "chat.js"
  get "/:chan" $ file "chat.html"
  post "/:chan" $ \chan -> do
    liftIO . putStrLn . show $ chan
    conn <- liftIO . connect $ defaultConnectInfo
    msg <- body
    let msg' = toByteString . fromLazyByteString $ msg
    liftIO . runRedis conn $ publish chan msg'
    text "ok"
  get "/:chan/stream" $ \chan -> do
    -- act as an EventSource stream
    header "Content-Type" "text/event-stream"
    -- create a connection to Redis
    conn <- liftIO . connect $ defaultConnectInfo
    -- create a conduit Source
    let stream = sourceRedisChannel conn chan
    source . sourceToSource $ (stream $= redis2EventSource) 

-- Redis Stuff
subscribe' :: B.ByteString -> Redis (Either R.Reply B.ByteString)
subscribe' channel = sendRequest ["SUBSCRIBE", channel]

recvMsg :: MonadIO m => Connection -> Source m PubSubReply
recvMsg conn = do
  r <- liftIO . runRedis conn $ recv
  yield . decodeMsg $ r

sourceRedisChannel conn chan = do
  liftIO . runRedis conn . subscribe' $ chan
  let loop = recvMsg conn >> loop
  loop

pubsubToBS (Msg (Message _ msg)) =
  fromByteString msg

redis2EventSource = CL.map $ 
  ServerEvent Nothing Nothing . return . pubsubToBS



