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
               , (Turn CW, "dqrvz1")
               , (Turn CCW, "kstuwx")
               ]

remaps :: Map Char Command
remaps = M.fromList $ concatMap (\(cmd, cs) -> map (, cmd) $ S.toList cs) $ M.toList maps

powerWords :: Map Solution (Set String)
powerWords = M.fromListWith S.union $ map (\s -> (map (remaps M.!) s, S.singleton s)) list
  where list = [ "ei!"
               , "ia!"
               , "r'lyeh"
               , "yuggoth"
               , "deep seven" --?
               , "chtonians" --?
               , "ph'nglui mglw'nafh cthulhu r'lyeh wgah'nagl fhtagn!"
               , "bigboote" --?
               , "tsathoggua" --?
               , "unnamable" --?
               , "yith" --?
               , "great race of yith" --?
               , "celeano" --?
               , "great hall of celeano" --?
               , "plateau of leng" --?
               , "abyss" --?
               , "lost carcosa" --?
               , "unknown kadath" --?
               , "the dreamlands" --?
               , "the underworld" --?
               , "dreamlands" --?
               , "underworlds" --?
               ]

findMaybe :: (a -> Maybe b) -> [a] -> Maybe b
findMaybe f = listToMaybe . mapMaybe f

checkWord :: [Command] -> Set String
checkWord cmd = foldr1 S.union $ map test $ M.toList powerWords
  where test (w, s) = if w `isPrefixOf` cmd
                      then s
                      else S.empty

checkWords :: [Command] -> [Set String]
checkWords = map checkWord . tails

wordify :: Solution -> String
wordify = map (S.findMin . (maps M.!))

dewordify :: String -> Solution
dewordify = map (remaps M.!)
