{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import Web.Scotty

import Data.Conduit
import qualified Data.Conduit.List as CL

import Data.Monoid (mconcat)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad (forM_)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C


import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.ByteString

import qualified Database.Redis as R
import Database.Redis.PubSub
import Database.Redis.Core

import Network.Wai.EventSource

import Control.Concurrent.Timer (repeatedTimer)
import Control.Concurrent.Suspend (sDelay)


main = scotty 8000 routes

file' ct fn = do
  header "Content-Type" ct
  file fn

routes = do
  -- if we don't send data to subscribers periodically,
  -- the http connection will tiome out.
  conn <- liftIO $ connect defaultConnectInfo
  liftIO $  repeatedTimer (keepAlive conn) (sDelay 10)

  get  "/channels"     listChannels
  get  "/:chan/stream" messageStream
  get  "/:chan/recent" recentMessages
  get  "/chat.css"   $ file' "text/css" "chat.css"
  get  "/chat.js"    $ file' "text/javascript" "chat.js"
  get  "/:chan"      $ file' "text/html" "chat.html"
  post "/:chan"        postMessage

postMessage chan = do
  conn <- liftIO . connect $ defaultConnectInfo
  msg <- body
  let msg' = toByteString . fromLazyByteString $ msg
  liftIO . runRedis conn $ do
    -- add exception handling
    R.sadd "channels" [chan]
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
    return . toMaybe $ results
  json messages


toMaybe = either (const Nothing) Just

listChannels = do
  conn <- liftIO $ connect defaultConnectInfo
  channels <- liftIO . runRedis conn $ R.smembers "channels"
  json . toMaybe $ channels

keepAlive conn = runRedis conn $ do
  Right chans <- R.smembers "channels"
  forM_ chans $ \chan ->
    publish chan ":keep alive"

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

pubsubToBS (Msg (Message _ msg)) = msg

redis2EventSource = CL.map $ quz . pubsubToBS

quz msg = if C.head msg == ':'
  then CommentEvent . fromByteString . C.tail $ msg
  else ServerEvent Nothing Nothing [fromByteString msg]
