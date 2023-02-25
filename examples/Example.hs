{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Main (main) where

import           Data.Aeson ( (.:) )
import           Data.ByteString.Lazy ( ByteString )
import           Control.Lens ( (^.), (.~), (&) )
import           Text.Feed.Types ( Feed (RSS1Feed) )
import qualified Data.Aeson       as Aeson
import qualified Network.Wreq     as Wreq
import qualified Text.RSS1.Syntax as RSS ( Item (..), Feed (..), nullChannel )
import qualified Data.Text as T
import qualified Text.HTML.TagSoup as TagSoup

import Rssify

example :: RssifyApp
example = FromHtml "https://news.ycombinator.com/news" parseHn settings
  where settings = RssifyAppSettings { refreshInterval = 10
                                     , appUrl = "/example"
                                     }

parseHn :: [TagSoup.Tag T.Text] -> Feed
parseHn tagList = buildHNFeed (uncurry toHNEntry <$> getTitlesAndLinks tagList)
  where getTitlesAndLinks :: [TagSoup.Tag T.Text] -> [(T.Text, T.Text)]
        getTitlesAndLinks ( TagSoup.TagOpen "span" [("class", "titleline")]
                  : TagSoup.TagOpen "a" [("href", link)]
                  : TagSoup.TagText title
                  : rest) = (title, link) : getTitlesAndLinks rest
        getTitlesAndLinks (_:xs) = getTitlesAndLinks xs
        getTitlesAndLinks [] = []

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
example2 = FromIO (mkFeed . parseEntries <$> getFeed) settings
  where settings = RssifyAppSettings { refreshInterval = 60
                                     , appUrl = "/example2"
                                     }

getFeed :: IO ByteString
getFeed = do
  resp <- Wreq.postWith options "https://www.iiss.org/api/filter" requestBody
  pure $ resp ^. Wreq.responseBody
    where options = Wreq.defaults
                  & Wreq.header "accept" .~ ["application/json"]
                  & Wreq.header "content-type" .~ ["application/json"]
          requestBody :: ByteString
          requestBody = "{\"templateId\":[\"{6BCFD2C9-4F0B-4ACE-95D7-D14C8B60CD4D}\"],\"componentId\":\"{E9850380-3707-43C9-994F-75ECE8048E04}\",\"page\":\"1\",\"amount\":10,\"filter\":{},\"tags\":null,\"sortType\":\"DateDesc\",\"restrictionType\":\"None\"}"

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
  { responseLink :: T.Text
  , responseTitle :: T.Text
  }

instance Aeson.FromJSON ResponseEntry where
  parseJSON (Aeson.Object v) = do
    link <- v .: "Link"
    title <- v .: "Heading"
    return $ ResponseEntry ("https://www.iiss.org" <> link) title

parseEntries :: ByteString -> [RSS.Item]
parseEntries bs = case Aeson.decode bs :: Maybe ResponseBody of
  Nothing -> []
  Just a  -> toItem <$> a.body
  where toItem :: ResponseEntry -> RSS.Item
        toItem entry = RSS.Item
          { RSS.itemURI = entry.responseLink
          , RSS.itemTitle = entry.responseTitle
          , RSS.itemLink = entry.responseLink
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
main = rssify [ example , example2 ]

