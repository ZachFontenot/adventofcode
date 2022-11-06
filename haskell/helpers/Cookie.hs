{-# LANGUAGE OverloadedStrings #-}

module Cookie (getInput) where

import Control.Lens ((&), (.~), (^.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as TE
import qualified Data.Text.Lazy.IO as TIO
import Network.HTTP.Client (Response)
import Network.Wreq (defaults, getWith)
import Network.Wreq.Lens (header, responseBody)
import System.Directory (doesFileExist)

readCookie :: IO ByteString
readCookie = do
  B.readFile "aoc_cookie"

handleResponse :: Response Lazy.ByteString -> FilePath -> IO T.Text
handleResponse response filename =
  let r = TE.decodeUtf8' $ response ^. responseBody
   in case r of
        Left _ -> pure "Something Blew Up" :: IO T.Text
        Right contents -> do
          putStrLn "Writing to new file"
          TIO.writeFile filename contents
          pure contents

getInput :: String -> String -> IO T.Text
getInput year day = do
  let filename = "inputs/" <> year <> "-" <> day <> ".txt" :: FilePath
  wellDoesIt <- doesFileExist filename
  if wellDoesIt
    then do
      putStrLn "Reading Cached File"
      TIO.readFile filename
    else do
      sessionVal <- readCookie
      let opts = defaults & header "Cookie" .~ ["session=" <> sessionVal]
      let uri = "https://adventofcode.com/" <> year <> "/day/" <> day <> "/input"
      r <- getWith opts uri
      handleResponse r filename
