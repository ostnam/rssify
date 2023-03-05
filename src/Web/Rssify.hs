{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NumDecimals #-}

module Web.Rssify
  ( rssify
  , rssify'
  , RssifyApp (..)
  , RssifyAppSettings (..)
  ) where

import           Control.Concurrent ( forkIO, threadDelay )
import           Control.Concurrent.STM.TVar ( TVar, newTVarIO, readTVarIO, writeTVar )
import           Control.Lens ( (^.) )
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.STM ( atomically )
import           Data.ByteString.Lazy (toStrict)
import           Data.Maybe ( fromMaybe )
import           Data.Text.Encoding ( decodeUtf8 )
import           Network.Wreq ( get, responseBody )
import           Text.Feed.Export ( textFeed )
import qualified Data.Text         as T ( Text )
import qualified Data.Text.Lazy    as TL ( Text )
import qualified Text.Feed.Types   as Feed
import qualified Text.HTML.TagSoup as TagSoup
import qualified Web.Scotty        as Scotty

-- | The main function: a Scotty server will be ran, serving the generated RSS
-- feeds to GET requests, at the specified path.
rssify :: [RssifyApp] -> IO ()
rssify apps = rssify' apps >>= Scotty.scotty 8000

-- | An alternative version of the previous function, that returns the
-- ScottyM ().
rssify' :: [RssifyApp] -> IO (Scotty.ScottyM ())
rssify' apps = join (map toScotty apps)
  where join = fmap sequence_ . sequence

data RssifyApp = FromHtml String -- ^ The URL of the page to fetch.
                          ([TagSoup.Tag T.Text] -> Feed.Feed) -- ^ The conversion function.
                          RssifyAppSettings
               | FromIO (IO Feed.Feed) -- ^ A function that will return a Feed.
                        RssifyAppSettings

data RssifyAppSettings = RssifyAppSettings
  { refreshInterval :: Int -- ^ Interval of feed updates, in minutes.
  , appUrl :: String       -- ^ Path to the feed, on the current domain.
  }


toScotty :: RssifyApp -> IO (Scotty.ScottyM ())
toScotty (FromHtml url fn settings) = host settings $ fn <$> getFeed url
toScotty (FromIO getter settings) = host settings getter

getFeed :: String -> IO [TagSoup.Tag T.Text]
getFeed url = do
  resp <- get url
  pure $ TagSoup.parseTags $ decodeUtf8 $ toStrict $ resp ^. responseBody

host :: RssifyAppSettings -> IO Feed.Feed -> IO (Scotty.ScottyM ())
host settings feedGetter = do
  initialFeed <- feedGetter
  feedRef <- newTVarIO $ fromMaybe "" $ textFeed initialFeed
  _ <- forkIO $ feedRefresh settings feedGetter feedRef
  return $ Scotty.get (Scotty.literal settings.appUrl) $ do
    feed <- liftIO $ readTVarIO feedRef
    Scotty.text feed

feedRefresh :: RssifyAppSettings -> IO Feed.Feed -> TVar TL.Text -> IO ()
feedRefresh settings getter tvar = do
  threadDelay $ 1.0e6 * 60 * settings.refreshInterval
  newFeed <- getter
  atomically $ writeTVar tvar (fromMaybe "" $ textFeed newFeed)
  feedRefresh settings getter tvar
