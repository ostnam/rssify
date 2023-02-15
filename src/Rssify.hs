{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

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
import Control.Concurrent.STM.TVar ( TVar, newTVarIO, readTVarIO )
import Data.Maybe ( fromMaybe )
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent ( forkIO )
import qualified Web.Scotty as Scotty
import qualified Text.HTML.TagSoup as TagSoup
import qualified Text.Feed.Types as Feed
import qualified Data.Text as T ( Text )
import qualified Data.Text.Lazy as TL ( Text )

-- todo
rssify :: [RssifyApp] -> Scotty.ScottyM ()
rssify = mapM_ toScotty

toScotty :: RssifyApp -> Scotty.ScottyM ()
toScotty (FromHtml url fn settings) = host settings $ fn <$> getFeed url
toScotty (FromRequest getter settings) = host settings getter

getFeed :: String -> IO [TagSoup.Tag T.Text]
getFeed url = do
  resp <- get url
  pure $ TagSoup.parseTags $ decodeUtf8 $ toStrict $ resp ^. responseBody

host :: RssifyAppSettings -> IO Feed.Feed -> Scotty.ScottyM ()
host settings feedGetter = do
  initialFeed <- feedGetter
  feedRef <- liftIO $ newTVarIO $ fromMaybe "" $ textFeed initialFeed
  _ <- liftIO . forkIO $ feedRefresh feedGetter feedRef
  Scotty.get (Scotty.literal settings.url) $ do
    feed <- liftIO $ readTVarIO feedRef
    Scotty.text feed
  
feedRefresh :: IO Feed.Feed -> TVar TL.Text -> IO ()
feedRefresh = undefined

data RssifyApp = FromHtml String ([TagSoup.Tag T.Text] -> Feed.Feed) RssifyAppSettings
               | FromRequest (IO Feed.Feed) RssifyAppSettings

data RssifyAppSettings = RssifyAppSettings
  { refreshInterval :: Int
  , url :: String
  }
