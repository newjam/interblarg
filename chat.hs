{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}

import Web.Scotty

import Data.Conduit
import qualified Data.Conduit.List as CL

-- Aeson instances for free
import Data.Data
import Data.Typeable


import Data.Monoid (mconcat)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad (forM_, void)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C


import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.ByteString

import qualified Database.Redis as R
import Database.Redis.PubSub
import Database.Redis.Core

import Network.Wai.EventSource
import Network.Wai

import Control.Concurrent.Timer (repeatedTimer)
import Control.Concurrent.Suspend (sDelay)

import Crypto.Hash (hash, digestToByteString, SHA1, SHA512, Digest)

import Data.String

import Data.ByteString.Base64 as B64

import Network.HTTP.Types.Header (hUserAgent)

import qualified Network.Socket as S

import qualified Data.Aeson.Generic as JSON

import Data.Semigroup

instance Semigroup B.ByteString where
  (<>) = mappend

main = scotty 8000 routes

data Blah = Blah
  { payload :: B.ByteString
  , colorId :: B.ByteString
  } deriving (Data, Typeable, Show)



file' ct fn = do
  header "Content-Type" ct
  file fn

routes = do
  -- if we don't send data to subscribers periodically,
  -- the http connection will time out.
  conn <- liftIO $ connect defaultConnectInfo
  liftIO $  repeatedTimer (keepAlive conn) (sDelay 10)

  get  "/identify"   $ request >>= json . identify
  get  "/channels"     listChannels
  get  "/:chan/stream" messageStream
  get  "/:chan/recent" recentMessages
  get  "/chat.css"   $ file' "text/css" "chat.css"
  get  "/chat.js"    $ file' "text/javascript" "chat.js"
  get  "/:chan"      $ file' "text/html" "chat.html"
  post "/:chan"        postMessage


fuck r = r >>= either 
  (\(R.Error x) -> error . show $ "you fucked redis: " <> x)
  return

fucking r = r >>= maybe
  (error "you thought you didn't fuck up but you did.")
  return

postMessage chan = do
  conn <- liftIO . connect $ defaultConnectInfo
  req <- request
  let poster = identify req
  msg <- body
  let lazy2Strict = toByteString . fromLazyByteString
  let msg' = lazy2Strict msg
  liftIO . runRedis conn $ do
    -- add exception handling
    R.sadd "channels" [chan]
    let posters = chan <> "-posters"
    -- if this is a new user assign them a number
    numRemoved <- fuck $ R.lrem posters 1 poster
    n <- fuck $ R.lpush posters [poster]

    let numColors = 16 
    i <- if n > numColors
    then do
      oldPoster <- fucking . fuck $ R.rpop posters
      oldNumber <- fucking . fuck $ R.get (chan <> "-" <> oldPoster)
      R.del [oldPoster]
      return oldNumber 
      --liftIO . print $ "end of cycle, last active poster: " <> oldPoster
    else
      return . fromString . show $ n

    let isNewPoster = numRemoved == 0
    x <- if isNewPoster 
    then do
      void $ R.set (chan <> "-" <> poster) i
      return i
    else fucking . fuck $ R.get (chan <> "-" <> poster)

    let msg'' = lazy2Strict . JSON.encode $ Blah msg' ("color" <> x)

    R.lpush chan [msg'']
    R.ltrim chan 0 9
    R.publish chan msg''
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

foob :: S.SockAddr -> B.ByteString
foob (S.SockAddrInet _  addr) = fromString . show $ addr
foob (S.SockAddrInet6 _ _ (a, b, c, d) _) = B.concat . map (fromString . show) $ [a,b,c,d]
foob (S.SockAddrUnix sock) = fromString sock

identify req = B64.encode . sha1. B.concat $ [user_agent, ip] where
    ip = fromString . show . foob . remoteHost $ req
    user_agent = maybe "" id . lookup hUserAgent .
        requestHeaders $ req

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

-- Crypto
sha1 :: B.ByteString -> B.ByteString
sha1 = digestToByteString . (hash :: B.ByteString -> Digest SHA1)

