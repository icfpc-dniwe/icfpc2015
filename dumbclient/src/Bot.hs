{-# LANGUAGE TupleSections #-}

module Bot where

import Data.List
import Data.Ord
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Linear.V2
import Debug.Trace

import Types
import Field

data PathTree = DeadEnd !HCells !Float
             | Crossroad !(Map Command PathTree)
             deriving (Show, Eq)

mergeSol :: PathTree -> PathTree -> PathTree
mergeSol a@(DeadEnd _ _) (DeadEnd _ _) = a
mergeSol (Crossroad a) (Crossroad b) = Crossroad (a `M.union` b)
mergeSol _ _ = error "mergeSol: impossible!"

type OldsSet = Set HCells

type Solutions = Map HCells (Solution, Float)

validPaths :: Field -> PathTree
validPaths (Field { unit = Nothing }) = error "validPaths: no unit on the field"
validPaths startf@(Field { unit = Just startu }) = myPath S.empty startf
  where path :: [Command] -> (OldsSet -> Field -> PathTree) -> OldsSet -> Field -> PathTree
        path _ _ _ (Field { unit = Nothing }) = error "path: no unit on the field"
        path cmds next olds f@(Field { unit = Just u }) =
          Crossroad $ M.fromList $ mapMaybe (\c -> (c, ) <$> (command c f >>= check)) cmds

          where olds' = S.insert (absCoords u) olds
                check f'@(Field { unit = Just u' })
                  | sourceLength f' /= sourceLength startf = Just $ DeadEnd (absCoords u) (score f' - startScore)
                  | absCoords u' `S.member` olds' = Nothing
                  | otherwise = Just $ next olds' f'
                check f'@(Field { unit = Nothing }) = Just $ DeadEnd (absCoords u) (score f' - startScore)

        cheight = maximum ys - minimum ys
          where ys = map ((\(V2 _ y) -> y) . hcellToCell) $ S.toList $ members startu
        startScore = score startf
        
        tryOr cmds next olds f = mergeSol (path cmds (tryOr cmds next) olds f) (next olds f)

        dropAll = path [Move SW, Move SE] dropAll
        dropSome 0 next = next
        dropSome n next = path [Move SW, Move SE] (dropSome (n - 1) next)
        turn = tryOr [Turn CW, Turn CCW]
        moveH = tryOr [Move W, Move E]

        myPath = moveH $ dropSome cheight $ turn $ dropAll
        --myPath = dropAll

solutions :: PathTree -> Solutions
solutions = sols []
  where sols cmds (DeadEnd cells scr) = M.singleton cells (reverse cmds, scr)
        sols cmds (Crossroad ts) = foldr M.union M.empty $ map (\(c, t) -> sols (c:cmds) t) $ M.toList ts

findBest :: HCells -> Solutions -> (Solution, Float)
findBest filled = maximumBy (comparing snd) . map (\(cells, (sol, int)) -> (sol, scorify cells sol int)) . M.toList
  where scorify cells sol scor = scor + fromIntegral (sum $ map ((\(V2 _ y) -> y) . hcellToCell) $ S.toList cells)

data Bot = Bot { solution :: Solution
               , unitNum :: Int
               }
         deriving (Show, Eq)

newBot :: Field -> Bot
newBot f = Bot { solution = fst $ findBest (filled f) $ solutions $ validPaths f
               , unitNum = sourceLength f
               }

advanceBot :: Field -> Bot -> (Command, Bot)
advanceBot f bot = (c, bot' { solution = cmds })
  where bot'@(Bot { solution = c:cmds })
          | sourceLength f /= unitNum bot = Bot { solution = fst $ findBest (filled f) $ solutions $ validPaths f
                                               , unitNum = sourceLength f
                                               }
          | otherwise = bot
