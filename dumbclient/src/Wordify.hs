{-# LANGUAGE TupleSections #-}

module Wordify where

import Control.Arrow
import Data.List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

import Types

maps :: Map Command (Set Char)
maps = M.fromList $ map (second S.fromList) list
  where list = [ (Move W, "p'!.03")
               , (Move E, "bcefy2")
               , (Move SW, "aghij4")
               , (Move SE, "lmno 5")
               , (Turn CW, "dqrvzl")
               , (Turn CCW, "kstuwx")
               ]

remaps :: Map Char Command
remaps = M.fromList $ concatMap (\(cmd, cs) -> map (, cmd) $ S.toList cs) $ M.toList maps

powerWords :: Map Solution String
powerWords = M.fromList $ map (\s -> (map conv s, s)) list
  where conv c = fst $ fromJust $ find (S.member c . snd) $ M.toList maps
        list = [ "ea!"
               ]

findMaybe :: (a -> Maybe b) -> [a] -> Maybe b
findMaybe f = listToMaybe . mapMaybe f

wordify :: Solution -> String
wordify = map (S.findMin . (maps M.!))

dewordify :: String -> Solution
dewordify = map (remaps M.!)
