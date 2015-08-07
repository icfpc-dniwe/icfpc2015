{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}

module Data where

import Control.Monad
import Network.Wreq
import Control.Lens ((^.))
import Data.Aeson
import Data.Aeson.TH
import Data.List
import GHC.Generics (Generic)

data Cell = Cell { x :: Integer
                 , y :: Integer
                 }
          deriving (Show, Eq, Generic, FromJSON)

data Unit = Unit { members :: [Cell]
                 , pivot :: Cell
                 }
          deriving (Show, Eq, Generic, FromJSON)

data RawInput = RawInput { id :: Integer
                         , units :: [Unit]
                         , width :: Integer
                         , height :: Integer
                         , filled :: [Cell]
                         , sourceLength :: Integer
                         , sourceSeeds :: [Integer]
                         }
             deriving (Show, Eq, Generic, FromJSON)

getInput :: Integer -> IO RawInput
getInput n = do
  r <- get $ "http://icfpcontest.org/problems/problem_" ++ show n ++ ".json"
  unless ((r^.responseStatus.statusCode) == 200) $ fail "getInput: failed"
  rs <- asJSON r
  return (rs^.responseBody)

data Direction = E | W | SE | SW
               deriving (Show, Eq)

data TDirection = CW | CCW
                deriving (Show, Eq)

data Command = Move Direction
             | Turn TDirection
             deriving (Show, Eq)

formatCmd :: Command -> Char
formatCmd (Move W) = 'p'
formatCmd (Move E) = 'b'
formatCmd (Move SW) = 'a'
formatCmd (Move SE) = 'l'
formatCmd (Turn CW) = 'd'
formatCmd (Turn CCW) = 'k'

data RawOutput = RawOutput { problemId :: Integer
                           , seed :: Integer
                           , tag :: String
                           , solution :: [Command]
                           }
               deriving (Show, Eq)

instance ToJSON RawOutput where
  toJSON x = object [ "problemId" .= problemId x
                    , "seed" .= seed x
                    , "tag" .= tag x
                    , "solution" .= map formatCmd (solution x)
                    ]

postOutput :: Integer -> [RawOutput] -> IO ()
postOutput teamid outputs = do
  r <- post ("https://davar.icfpcontest.org/teams/" ++ show teamid ++ "/solutions/") (toJSON outputs)
  unless ((r^.responseStatus.statusCode) == 200) $ fail "postOutput: failed"
