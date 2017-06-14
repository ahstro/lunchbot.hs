module Main where

import Data.Time (utctDay)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.Time.Clock (getCurrentTime)

import Network.HTTP (getRequest, getResponseBody, simpleHTTP)

cremeURL :: String
cremeURL = "http://bistro.creme.se/meny"

openURL :: String -> IO String
openURL url = getResponseBody =<< simpleHTTP (getRequest url)

main :: IO ()
main = do
  src <- openURL cremeURL
  time <- getCurrentTime
  let (_, _, weekday) = toWeekDate $ utctDay time
  putStrLn $ show weekday
