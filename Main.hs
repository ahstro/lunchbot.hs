module Main where

import Data.Char (isAlpha, toLower)
import Data.Time (utctDay)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.Time.Clock (getCurrentTime)
import Network.HTTP (getRequest, getResponseBody, simpleHTTP)
import Text.HTML.TagSoup

cremeURL :: String
cremeURL = "http://bistro.creme.se/meny"

openURL :: String -> IO String
openURL url = getResponseBody =<< simpleHTTP (getRequest url)

main :: IO ()
main = do
  src <- openURL cremeURL
  time <- getCurrentTime
  let (_, _, weekday) = toWeekDate $ utctDay time
  let tags = parseTags src
  putStrLn $ getMenu "1316" tags
  putStrLn $ getMenu "1317" tags
  putStrLn $ getMenu "1353" tags
  putStrLn $ getMenu "1354" tags
  putStrLn $ getMenu "1355" tags
  where
    getMenu id =
      unlines .
      filter ((> 5) . length . words) .
      map capitalize .
      map unwords .
      filter ((> 0) . length) .
      map words .
      lines .
      innerText .
      takeWhile (~/= "<div class=media-themed>") .
      dropWhile (~/= ("<div id=category-" ++ id ++ "-courses>"))
    capitalize (head:tail) = head : map toLower tail
