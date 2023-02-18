{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Rssify
import Text.Feed.Types ( Feed (RSS1Feed) )
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


main :: IO ()
main = rssify [ example
              ]

