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

main = scotty 8000 routes

routes = do
  get  "/chat.css"   $ file "chat.css"
  get  "/chat.js"    $ file "chat.js"
  get  "/:chan"      $ file "chat.html"
  post "/:chan"        postMessage
  get  "/:chan/stream" messageStream
  get  "/:chan/recent" recentMessages


postMessage chan = do
  conn <- liftIO . connect $ defaultConnectInfo
  msg <- body
  let msg' = toByteString . fromLazyByteString $ msg
  liftIO . runRedis conn $ do
    -- add exception handling
    R.lpush chan [msg']
    R.ltrim chan 0 9
    R.publish chan msg'
  text "ok"

messageStream chan = do
  -- act as an EventSource stream
  header "Content-Type" "text/event-stream"
  -- create a connection to Redis
  conn <- liftIO . connect $ defaultConnectInfo
  -- create a conduit Source
  let stream = sourceRedisChannel conn chan
  source . sourceToSource $ (stream $= redis2EventSource) 

recentMessages chan = do 
  conn <- liftIO . connect $ defaultConnectInfo
  messages <- liftIO . runRedis conn $ do
    results <- R.lrange chan 0 9
    let toMaybe = either (const Nothing) Just
    return . toMaybe $ results
  json messages

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

