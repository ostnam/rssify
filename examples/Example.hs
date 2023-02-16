{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Rssify
import Text.RSS.Syntax
import Text.Feed.Types

example :: RssifyApp
example = FromHtml "https://www.google.com" fn settings
  where fn _ = RSSFeed $ RSS "1.0" [] (nullChannel "example" "/example") [] 
        settings = RssifyAppSettings 10 "/example"

example2 :: RssifyApp
example2 = FromHtml "https://www.google.com" fn settings
  where fn _ = RSSFeed $ RSS "1.0" [] (nullChannel "example 2" "/example") [] 
        settings = RssifyAppSettings 10 "/example2"

main :: IO ()
main = rssify [ example
              , example2
              ]
                
