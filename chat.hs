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

import Data.ByteString.Lazy (fromChunks)

import qualified Database.Redis as R
import Database.Redis.PubSub
import Database.Redis.Core

import Network.Wai.EventSource.EventStream
import Network.Wai.EventSource
import Network.Wai

import Control.Concurrent.Timer (repeatedTimer)
import Control.Concurrent.Suspend (sDelay)

import Crypto.Hash (hash, digestToByteString, SHA1, SHA512, Digest)

import Data.String

import qualified Data.ByteString.Base64.URL as B64

import Network.HTTP.Types.Header (hUserAgent)

import qualified Network.Socket as S

import Data.Maybe (mapMaybe)

import qualified Data.Aeson.Generic as GSON
import Data.Aeson (FromJSON(..), ToJSON(..), (.=), object, encode, decode)

import Data.Semigroup
import Control.Applicative
import Control.Monad

import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Time.Clock
{- newtype MyRedis a = Redis (Either Reply a)

get :: RedisCtx

instance MonadRedis MyRedis where
  liftRedis :: 

I want something like

add :: ByteString -> ByteString ->  MyRedis (Maybe Int)
add k1 k2 = do
  ma <- R.get k1 
  mb <- R.get k2
  return $ (+) <$> ma <*> mb
  

-}


instance Semigroup B.ByteString where
  (<>) = mappend

main = scotty 8000 routes

data ChatMsg = ChatMsg
  { payload :: B.ByteString
  , user :: B.ByteString
  , time :: Integer
  } deriving (Data, Typeable, Show)

instance ToJSON ChatMsg where
  toJSON = GSON.toJSON

file' ct fn = do
  header "Content-Type" ct
  file fn

routes = do
  -- if we don't send data to subscribers periodically,
  -- the http connection will time out.
  conn <- liftIO $ connect defaultConnectInfo
  liftIO $  repeatedTimer (keepAlive conn) (sDelay 10)

  get  "/identify"     $ request >>= json . identify
  get  "/channels"       listChannels
  get  "/:chan/stream"   messageStream
  get  "/:chan/recent"   recentMessages
  get  "/:chan/me"       getMe
  post "/:chan/me"       changeMe
  get  "/:chan/:poster"  userInfo
  get  "/chat.css"     $ file' "text/css" "chat.css"
  get  "/chat.js"      $ file' "text/javascript" "chat.js"
  get  "/:chan"        $ file' "text/html" "chat.html"
  post "/:chan"          postMessage




fuck r = r >>= either 
  (\(R.Error x) -> error . show $ "you fucked redis: " <> x)
  return

fucking r = r >>= maybe
  (error "you thought you didn't fuck up but you did.")
  return

{--
 
 on post, parse input and validate it so we don't publish funky or maliscious
 messages such as <script>alert("loser!")</script>

 also: refactor. this is ugly.

 timestamp messages

 allow for multiple streams at once, ie /stream1+stream2 would merge
 the two, sorting by timestamp, posting would publish to both.
 How to resolve the colors?

 be able to upload images to amazon cdn

 ooooh! refactor, make a function getOrAssignColor
 and instead of saving the color with the msg, just
 save the user. when loading messages on client
 get the color of the user, and color the post that.

 IMPORTANT: This leaves orphan subscriptions to redis.
 If this were real, that would be pretty important to fix.
 if the http connection is terminated, everything else should too.

--}

{-

input: chan, client

add poster to chan's set of clients
if client hasn't posted in chan before, assign them a new id, the card of the 
associate that client with that id

-}

foobar chan poster = do
  let chan' = "chan[" <> chan <> "]"
  let posters' = (chan' <> ".posters")
  let poster' = posters' <> "[" <> poster <> "]" -- posters[" <> poster <> "]"
  i <- fuck $ R.sadd posters' [poster]
  if i == 1
  then do
    -- get the number associated with this poster
    fucking . fuck $ R.get poster'
  else do-- add
    n <- fuck $ R.scard posters'
    let n' = fromString . show $ n 
    void $ R.set poster' n'
    return n'

-- assume user does not have a color.
-- assign user a color if one is available
-- otherwise, take the color of an old user.
getOrAssignColor chan poster = do
  let posters = "chan["<>chan<>"].users" -- <> "-posters"
  -- if this is a new user assign them a number
  numRemoved <- fuck $ R.lrem posters 1 poster
  numPosters <- fuck $ R.lpush posters [poster]

  let toKey p = "chan["<>chan<>"].users["<>p<>"].color" --   chan <> "-" <> p

  let numColors = 16 
  i <- if numPosters > numColors
  then do
    oldPoster <- fucking . fuck $ R.rpop posters
    oldNumber <- fucking . fuck $ R.get (toKey oldPoster)
    R.del [oldPoster]
    return oldNumber 
    --liftIO . print $ "end of cycle, last active poster: " <> oldPoster
  else
    return . fromString . show $ numPosters

  let isNewPoster = numRemoved == 0
  if isNewPoster 
  then do
    void $ R.set (toKey poster) i
    return i
  else fucking . fuck $ R.get (toKey poster)

--getName chan poster color = return $ "user" <> color
  
lazy2Strict = toByteString . fromLazyByteString

data User = User {
    userId :: C.ByteString
  , userColor :: Maybe C.ByteString
  , userName :: Maybe C.ByteString
}

instance ToJSON User where
  toJSON (User id color name) = object ["id" .= id, "color" .= color, "name" .= name]

changeMe :: C.ByteString -> ActionM ()
changeMe chan = do
  userId <- identify <$> request
  let nameKey = "chan["<>chan<>"].users["<>userId<>"].name"
  newName <- lazy2Strict <$> body
  conn <- liftIO . connect $ defaultConnectInfo
  redis conn $ do
    R.set nameKey newName


    -- publish msg to all clients
    let msg = lazy2Strict . encode $ User userId Nothing (Just newName)
    let event = serverEvent "updateUser" msg
    R.publish chan event
  text "ok"


getOrCreateName :: C.ByteString -> C.ByteString -> Redis C.ByteString
getOrCreateName chan userId = getName >>= maybe assignName return where
  getName = fuck $ R.get nameKey
  assignName = do
    n <- fuck $ R.incr counterKey
    let name = fromString $ "user" ++ (show n)
    R.set nameKey name
    return name
  base = "chan["<>chan<>"]"
  nameKey = base<>".users["<>userId<>"].name"
  counterKey = base<>".nameCounter"



userInfo' :: C.ByteString -> C.ByteString -> Redis User
userInfo' chan user = User user <$> color <*> name where
  color = Just <$> getOrAssignColor chan user
  name  = Just <$> getOrCreateName chan user


redis :: Connection -> Redis a -> ActionM a
redis conn = liftIO . runRedis conn

userInfo chan userId = do
  conn <- liftIO . connect $ defaultConnectInfo
  info <- redis conn $ userInfo' chan userId
  json info

getMe = identify <$> request >>= json

instance Applicative ActionM where
  pure  = return
  (<*>) = ap


-- |'extractMessage' constructs a 'ChatMsg' from the http post request
extractMessage :: ActionM ChatMsg
extractMessage = ChatMsg     <$> payload <*> user <*> time where
  payload  = lazy2Strict <$> body
  user     = identify    <$> request
  time     = truncate    <$> liftIO getPOSIXTime

-- |publish 'msg' to 'chan' via redis
publishMessage' :: C.ByteString -> ChatMsg -> Redis Integer
publishMessage' chan msg = do
  -- add this channel to the set of all channels
  -- [not used yet, but interesting]
  R.sadd "channels" [chan]

  let encodedMsg = lazy2Strict . encode $ msg

  -- add msg to persistent list of msgs
  R.lpush chan [encodedMsg]
  -- keep only 100 msgs
  R.ltrim chan 0 99

  -- translate message into valid text/event-source syntax
  let event = serverEvent "newMessage" encodedMsg

  -- publish msg to all clients
  fuck $ R.publish chan event

postMessage chan = do
  conn    <- liftIO . connect $ defaultConnectInfo
  let publishMessage = redis conn . publishMessage' chan 
  listeners <- extractMessage >>= publishMessage
  debug $ "postMessage " <> (C.pack . show $ listeners)
  text "ok"

debug x = liftIO . putStrLn . C.unpack $ x


{-

>>>> data model <<<<

have a client side function that turns a user # into 
a color. Have it always return the same color for each client
Decouple data from presentation. except the user can't change the color :(

post
  payload
  user

this is a goal, not the current reality

list of messages in channel

  endpoint
    /test/messages

  redis key
    channels.test.messages

  value
    [8,7,6,5]

single message in channel
      
  endpoint
    /test/messages/8

  redis key
    channels.test.messages.8

  value
    {
      payload:"Hello, world.",
      user:"5ag5fcsw6jgfh90g3df"
    }

single user from channel

  endpoint
    /test/users/5ag5fcsw6jgfh90g3df

  redis key
    channels.test.users.5ag5fcsw6jgfh90g3df

  value
    {
      color:0,
      name:"James"
    }

-}


messageStream chan = do
  -- act as an EventSource stream
  header "Content-Type" "text/event-stream"
  -- create a connection to Redis
  conn <- liftIO . connect $ defaultConnectInfo
  -- Source of Bytestring from listening to a Redis pubsub channel
  let stream  = sourceRedisChannel conn chan
  -- turn Source of ByteString into Source of  Flush Builder
  let builder = CL.concatMap $ \x -> [Chunk $ insertByteString x, Flush]
  
  debug "subscribed"

  source $ stream $= builder 

recentMessages' :: C.ByteString -> Redis [ChatMsg]
recentMessages' chan = do
  rawMessages <- fuck $ R.lrange chan 0 99
  let decodeMessage :: C.ByteString -> Maybe ChatMsg
      decodeMessage = GSON.decode . fromChunks . return
      messages = mapMaybe decodeMessage rawMessages
  return messages

recentMessages chan = do 
  conn <- liftIO . connect $ defaultConnectInfo
  messages <- redis conn $ recentMessages' chan
  json messages

foo :: ServerEvent -> C.ByteString
foo =  maybe "" toByteString . eventToBuilder

addr :: S.SockAddr -> B.ByteString
addr (S.SockAddrInet _  addr) = fromString . show $ addr
addr (S.SockAddrInet6 _ _ (a, b, c, d) _) = B.concat . map (fromString . show) $ [a,b,c,d]
addr (S.SockAddrUnix sock) = fromString sock

identify req = B64.encode . sha1. B.concat $ [user_agent, ip] where
    ip = fromString . show . addr . remoteHost $ req
    user_agent = maybe "" id . lookup hUserAgent .
        requestHeaders $ req

toMaybe = either (const Nothing) Just

listChannels = do
  conn <- liftIO $ connect defaultConnectInfo
  channels <- redis conn $ R.smembers "channels"
  json . toMaybe $ channels

keepAlive conn = runRedis conn $ do
  Right chans <- R.smembers "channels"
  forM_ chans $ \chan ->
    publish chan . serverComment $ "keep alive"

-- Redis Stuff
subscribe' :: B.ByteString -> Redis (Either R.Reply B.ByteString)
subscribe' channel = sendRequest ["SUBSCRIBE", channel]

recvMsg conn = do
  r <- liftIO . runRedis conn $ recv
  yield . pubsubToBS . decodeMsg $ r


sourceRedisChannel conn chan = do
  liftIO . runRedis conn . subscribe' $ chan
  let loop = recvMsg conn >> loop
  loop

pubsubToBS (Msg (Message _ msg)) = msg

serverEvent n d = foo $ ServerEvent n' Nothing [d'] where
  n' = Just . fromByteString . C.cons ' ' $ n
  d' = fromByteString $ d
serverComment   = foo . CommentEvent . fromByteString 

-- Crypto
sha1 :: B.ByteString -> B.ByteString
sha1 = digestToByteString . (hash :: B.ByteString -> Digest SHA1)

