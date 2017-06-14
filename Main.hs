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

getId :: Int -> Maybe String
getId 1 = Just "1316"
getId 2 = Just "1317"
getId 3 = Just "1353"
getId 4 = Just "1354"
getId 5 = Just "1355"
getId _ = Nothing

main :: IO ()
main = do
  src <- openURL cremeURL
  time <- getCurrentTime
  let (_, _, weekday) = toWeekDate $ utctDay time
  let tags = parseTags src
  case getId weekday of
    Just id -> putStrLn $ getMenu id tags
    Nothing -> putStrLn "No creme today"
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
