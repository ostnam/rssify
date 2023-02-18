{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NumDecimals #-}

module Rssify
  ( rssify
  , RssifyApp (..)
  , RssifyAppSettings (..)
  ) where

import Data.Text.Encoding ( decodeUtf8 )
import Network.Wreq ( get, responseBody )
import Control.Lens ( (^.) )
import Data.ByteString.Lazy (toStrict)
import Text.Feed.Export ( textFeed )
import Control.Concurrent.STM.TVar ( TVar, newTVarIO, readTVarIO, writeTVar )
import Control.Monad.STM ( atomically )
import Data.Maybe ( fromMaybe )
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent ( forkIO, threadDelay )
import qualified Web.Scotty as Scotty
import qualified Text.HTML.TagSoup as TagSoup
import qualified Text.Feed.Types as Feed
import qualified Data.Text as T ( Text )
import qualified Data.Text.Lazy as TL ( Text )

rssify :: [RssifyApp] -> IO ()
rssify apps = join (map toScotty apps) >>= Scotty.scotty 8000

join :: [IO (Scotty.ScottyM ())] -> IO (Scotty.ScottyM ())
join = fmap sequence_ . sequence
-- sequence gives IO [ScottyM ()], fmapping sequence_ over it gives a IO ScottyM ()

toScotty :: RssifyApp -> IO (Scotty.ScottyM ())
toScotty (FromHtml url fn settings) = host settings $ fn <$> getFeed url
toScotty (FromRequest getter settings) = host settings getter

getFeed :: String -> IO [TagSoup.Tag T.Text]
getFeed url = do
  resp <- get url
  pure $ TagSoup.parseTags $ decodeUtf8 $ toStrict $ resp ^. responseBody

host :: RssifyAppSettings -> IO Feed.Feed -> IO (Scotty.ScottyM ())
host settings feedGetter = do
  initialFeed <- feedGetter
  feedRef <- newTVarIO $ fromMaybe "" $ textFeed initialFeed
  _ <- forkIO $ feedRefresh settings feedGetter feedRef
  return $ Scotty.get (Scotty.literal settings.url) $ do
    feed <- liftIO $ readTVarIO feedRef
    Scotty.text feed

feedRefresh :: RssifyAppSettings -> IO Feed.Feed -> TVar TL.Text -> IO ()
feedRefresh settings getter tvar = do
  threadDelay $ 1.0e6 * 60 * settings.refreshInterval
  newFeed <- getter
  atomically $ writeTVar tvar (fromMaybe "" $ textFeed newFeed)
  feedRefresh settings getter tvar


data RssifyApp = FromHtml String ([TagSoup.Tag T.Text] -> Feed.Feed) RssifyAppSettings
               | FromRequest (IO Feed.Feed) RssifyAppSettings

data RssifyAppSettings = RssifyAppSettings
  { refreshInterval :: Int
  , url :: String
  }
