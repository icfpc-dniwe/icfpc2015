{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}

module ReadWrite where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Set (Set)
import Control.Monad
import Network.Wreq
import Control.Lens hiding ((.=))
import Data.Aeson
import GHC.Generics (Generic)

import Types

data Unit = Unit { members :: Set Cell
                 , pivot :: Cell
                 }
          deriving (Show, Eq, Generic, FromJSON)

data RawInput = RawInput { id :: Integer
                         , units :: [Unit]
                         , width :: Int
                         , height :: Int
                         , filled :: Set Cell
                         , sourceLength :: Int
                         , sourceSeeds :: [Integer]
                         }
             deriving (Show, Eq, Generic, FromJSON)

getInput :: Integer -> IO RawInput
getInput n = do
  r <- get $ "http://icfpcontest.org/problems/problem_" ++ show n ++ ".json"
  unless ((r^.responseStatus.statusCode) == 200) $ fail "getInput: failed"
  rs <- asJSON r
  return (rs^.responseBody)

data RawOutput = RawOutput { problemId :: Integer
                           , seed :: Integer
                           , tag :: Text
                           , solution :: String
                           }
               deriving (Show, Eq, Generic, ToJSON)

postOutput :: Integer -> ByteString -> [RawOutput] -> IO ()
postOutput teamid token outputs = do
  let opts = defaults & auth ?~ basicAuth "" token
      url = "https://davar.icfpcontest.org/teams/" ++ show teamid ++ "/solutions/"
  r <- postWith opts url (toJSON outputs)
  unless ((r^.responseStatus.statusCode) == 200) $ fail "postOutput: failed"
