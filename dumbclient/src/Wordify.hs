{-# LANGUAGE TupleSections #-}

module Wordify where

import Control.Arrow
import Data.List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.List
import Data.Ord

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
  where list = sortOn Down [ "ei!"
               , "ia! ia!"
               , "tsathoggua"
               , "r'lyeh"
               , "yuggoth"
               , "necronomicon"
               , "ph'nglui mglw'nafh cthulhu r'lyeh wgah'nagl fhtagn!"
               , "john bigboote"
               , "yogsothoth"
               ]

findMaybe :: (a -> Maybe b) -> [a] -> Maybe b
findMaybe f = listToMaybe . mapMaybe f

checkWord :: Solution -> Set String
checkWord cmd = foldr1 S.union $ map test $ M.toList powerWords
  where test (w, s) = if w `isPrefixOf` cmd
                      then s
                      else S.empty

checkWords :: Solution -> [Set String]
checkWords = map checkWord . tails

wordifyDumb :: Solution -> String
wordifyDumb sol = foldr1 (++) $ map (pure . S.findMin) $ map (maps M.!) sol

wordify' :: Solution -> String -> String
wordify' [] str = str
wordify' sol@(h:t) str =
  let pw_set = checkWord sol in
    if pw_set /= S.empty then
      let max_pw = S.findMax pw_set in
        wordify' (drop (length max_pw) sol) $ str ++ max_pw
    else
      wordify' t (str ++ wordifyDumb [h])

wordify :: Solution -> String
wordify sol = wordify' sol ""

dewordify :: String -> Solution
dewordify = map (remaps M.!)
