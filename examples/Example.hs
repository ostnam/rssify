{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Main (main) where

import Rssify
import Text.Feed.Types ( Feed (RSS1Feed) )
import Control.Lens ( (^.), (.~), (&) )
import Data.ByteString.Lazy ( ByteString )
import Data.Aeson ( (.:) )
import qualified Data.Aeson as Aeson
import qualified Network.Wreq as Wreq
import qualified Text.RSS1.Syntax as RSS ( Item (..), Feed (..), nullChannel )
import qualified Data.Text as T
import qualified Text.HTML.TagSoup as TagSoup

example :: RssifyApp
example = FromHtml "https://news.ycombinator.com/news" parseHn settings
  where settings = RssifyAppSettings { refreshInterval = 10
                                     , url = "/example"
                                     }

parseHn :: [TagSoup.Tag T.Text] -> Feed
parseHn tagList = buildHNFeed (uncurry toHNEntry <$> getTitles tagList)
  where getTitles :: [TagSoup.Tag T.Text] -> [(T.Text, T.Text)]
        getTitles ( TagSoup.TagOpen "span" [("class", "titleline")]
                  : TagSoup.TagOpen "a" [("href", link)]
                  : TagSoup.TagText title
                  : rest) = (title, link) : getTitles rest
        getTitles (_:xs) = getTitles xs
        getTitles [] = []

        toHNEntry :: T.Text -> T.Text -> RSS.Item
        toHNEntry title link = RSS.Item
          { RSS.itemURI = link
          , RSS.itemTitle = title
          , RSS.itemLink = link
          , RSS.itemDesc = Nothing
          , RSS.itemDC = []
          , RSS.itemTopics = []
          , RSS.itemContent = []
          , RSS.itemOther = []
          , RSS.itemAttrs = []
          }

        buildHNFeed :: [RSS.Item] -> Feed
        buildHNFeed items = RSS1Feed $ RSS.Feed
          { RSS.feedVersion = "1"
          , RSS.feedChannel = RSS.nullChannel "" "hn"
          , RSS.feedImage = Nothing
          , RSS.feedItems = items
          , RSS.feedTextInput = Nothing
          , RSS.feedTopics = []
          , RSS.feedOther = []
          , RSS.feedAttrs = []
          }

example2 :: RssifyApp
example2 = FromRequest (mkFeed . parseEntries <$> getFeed) settings
  where settings = RssifyAppSettings { refreshInterval = 60
                                     , url = "/example2"
                                     }

getFeed :: IO ByteString
getFeed = do
  resp <- Wreq.postWith options "https://www.iiss.org/api/filter" body
  pure $ resp ^. Wreq.responseBody
    where options = Wreq.defaults
                  & Wreq.header "accept" .~ ["application/json"]
                  & Wreq.header "content-type" .~ ["application/json"]
          body :: ByteString
          body = "{\"templateId\":[\"{6BCFD2C9-4F0B-4ACE-95D7-D14C8B60CD4D}\"],\"componentId\":\"{E9850380-3707-43C9-994F-75ECE8048E04}\",\"page\":\"1\",\"amount\":10,\"filter\":{},\"tags\":null,\"sortType\":\"DateDesc\",\"restrictionType\":\"None\"}"

data ResponseBody = ResponseBody
  { body :: [ResponseEntry]
  }

instance Aeson.FromJSON ResponseBody where
  parseJSON (Aeson.Object v) = do
    model <- v .: "model"
    results <- model .: "Results"
    entries <- Aeson.parseJSON results
    return $ ResponseBody entries


data ResponseEntry = ResponseEntry
  { link :: T.Text
  , title :: T.Text
  }

instance Aeson.FromJSON ResponseEntry where
  parseJSON (Aeson.Object v) = do
    link <- v .: "Link"
    title <- v .: "Heading"
    return $ ResponseEntry ("https://www.iiss.org" <> link) title

parseEntries :: ByteString -> [RSS.Item]
parseEntries bs = case (Aeson.decode bs) :: Maybe ResponseBody of
  Nothing -> []
  Just a  -> toItem <$> a.body
  where toItem :: ResponseEntry -> RSS.Item
        toItem entry = RSS.Item
          { RSS.itemURI = entry.link
          , RSS.itemTitle = entry.title
          , RSS.itemLink = entry.link
          , RSS.itemDesc = Nothing
          , RSS.itemDC = []
          , RSS.itemTopics = []
          , RSS.itemContent = []
          , RSS.itemOther = []
          , RSS.itemAttrs = []
          }

mkFeed :: [RSS.Item] -> Feed
mkFeed items = RSS1Feed $ RSS.Feed
  { RSS.feedVersion = "1"
  , RSS.feedChannel = RSS.nullChannel "" "hn"
  , RSS.feedImage = Nothing
  , RSS.feedItems = items
  , RSS.feedTextInput = Nothing
  , RSS.feedTopics = []
  , RSS.feedOther = []
  , RSS.feedAttrs = []
  }


main :: IO ()
main = rssify [ example
              , example2
              ]

