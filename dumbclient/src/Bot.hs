{-# LANGUAGE TupleSections, MultiWayIf #-}

module Bot where

import Data.List
import Data.Ord
import Control.Monad.State.Strict
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Linear.V2
import Linear.V3

import Types
import Field

import Debug.Trace

data PathTree = DeadEnd !HCells !Field
              | Crossroad !(Map Command PathTree)
              deriving (Show, Eq)

mergeSol :: PathTree -> PathTree -> PathTree
mergeSol a@(DeadEnd _ _) (DeadEnd _ _) = a
mergeSol (Crossroad a) (Crossroad b) = Crossroad (a `M.union` b)
mergeSol _ _ = error "mergeSol: impossible!"

type OldsSet = Set HCells

data SolutionInfo = SolutionInfo { solutionCmds :: !Solution
                                 , newField :: !Field
                                 }
                  deriving (Show)

type Solutions = Map HCells SolutionInfo

mapMaybeM :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM _ [] = return []
mapMaybeM f (h:t) = do
  r <- f h
  case r of
   Nothing -> mapMaybeM f t
   Just x -> (x :) <$> mapMaybeM f t

pathTree :: Field -> PathTree
pathTree (Field { unit = Nothing }) = error "pathTree: no unit on the field"
pathTree startf@(Field { unit = Just startu }) = evalState (myPath startf) S.empty
  where path :: [Command] -> (Field -> State OldsSet PathTree) -> Field -> State OldsSet PathTree
        path _ _ (Field { unit = Nothing }) = error "path: no unit on the field"
        path cmds next f@(Field { unit = Just u }) = do
          modify $ S.insert $ absCoords u
          Crossroad <$> M.fromList <$> mapMaybeM (\c -> fmap (c, ) <$> check (command c f)) cmds

          where 
                dead f' = Just $ DeadEnd (absCoords u) f'

                check f'@(Field { unit = Just u' }) = do
                  olds <- get
                  if | sourceLength f' /= sourceLength startf -> return $ dead f'
                     | absCoords u' `S.member` olds -> return Nothing
                     | otherwise -> Just <$> next f'
                check f'@(Field { unit = Nothing }) = return $ dead f'

        cheight = maximum ys - minimum ys
          where ys = map (\(V3 _ _ z) -> z) $ S.toList $ members startu
        startScore = score startf
        
        tryOr cmds next f = mergeSol <$> path cmds (tryOr cmds next) f <*> next f

        dropAll = path [Move SW, Move SE] dropAll
        dropSome 0 next = next
        dropSome n next = path [Move SW, Move SE] (dropSome (n - 1) next)
        turn = tryOr [Turn CW, Turn CCW]
        moveH = tryOr [Move W, Move E]

        myPath = moveH $ dropSome cheight $ turn $ dropAll

-- TODO: can drop this to optimize further
solutions :: PathTree -> Solutions
solutions = sols []
  where sols cmds (DeadEnd cells f) = M.singleton cells $ SolutionInfo { solutionCmds = reverse cmds
                                                                       , newField = f
                                                                       }
        sols cmds (Crossroad ts) = foldr M.union M.empty $ map (\(c, t) -> sols (c:cmds) t) $ M.toList ts

bests :: Field -> [SolutionInfo]
bests field = map fst $ sortOn (Down . snd) $ map transform $ M.toList $ solutions $ pathTree field
  where transform (cells, si) = (si, scorify cells (solutionCmds si) (score (newField si) - score field))

        scorify cells sol scor = a1 * scor + 
          a2 * low cells + 
          a3 * whole +
          a4 * bump

        bumpiness (a:b:hs) = abs (a - b) + bumpiness (b:hs)
        bumpiness _ = 0

        cols' = M.fromListWith min $ map ((\(V2 x y) -> (x, y)) . hcellToCell) $ S.toList $ filled field
        cols = map snd $ M.toAscList $ cols' `M.union` M.fromList (zip [0..width field - 1] (repeat $ height field - 1))

        low cells = fromIntegral $ sum $ map (\(V3 _ _ z) -> z) $ S.toList cells
        whole = sum $ map (\c -> if S.null $ neighbors c S.\\ filled field then 1 else 0) $ S.toList $ filled field
        bump = fromIntegral $ bumpiness cols

        a1 = 0.02
        a2 = 0.01
        a3 = 30.0
        a4 = -50.0

data GameTree = GDeadEnd
              | GCrossroad !Float !(Map Solution GameTree)
              deriving (Show, Eq)

gameTree :: Field -> GameTree
gameTree startField = gt (score startField) startField
  where gt _ field@(Field { unit = Nothing }) = GDeadEnd
        gt decr field@(Field { unit = Just u }) =
          GCrossroad (score field - decr) $ M.fromList $ map transform $ take bestN $ bests field 

          where transform si = (solutionCmds si, gt (score field) (newField si))
                bestN = 10



data Bot = Bot { solution :: !Solution
               , unitNum :: !Int
               }
         deriving (Show, Eq)

newBot :: Field -> Bot
newBot f = Bot { solution = solutionCmds $ head $ bests f
               , unitNum = sourceLength f
               }

advanceBot :: Field -> Bot -> (Command, Bot)
advanceBot f bot = (c, bot' { solution = cmds })
  where bot'@(Bot { solution = c:cmds })
          | sourceLength f /= unitNum bot = Bot { solution = solutionCmds $ head $ bests f
                                               , unitNum = sourceLength f
                                               }
          | otherwise = bot
