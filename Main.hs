module Main where

import Network.HTTP (getRequest, getResponseBody, simpleHTTP)

cremeURL :: String
cremeURL = "http://bistro.creme.se/meny"

openURL :: String -> IO String
openURL url = getResponseBody =<< simpleHTTP (getRequest url)

main :: IO ()
main = do
  src <- openURL cremeURL
  putStrLn src
