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

data PathTree = DeadEnd !HCells !Float
              | Crossroad !(Map Command PathTree)
              deriving (Show, Eq)

mergeSol :: PathTree -> PathTree -> PathTree
mergeSol a@(DeadEnd _ _) (DeadEnd _ _) = a
mergeSol (Crossroad a) (Crossroad b) = Crossroad (a `M.union` b)
mergeSol _ _ = error "mergeSol: impossible!"

type OldsSet = Set HCells

type Solutions = Map HCells (Solution, Float)

mapMaybeM :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM _ [] = return []
mapMaybeM f (h:t) = do
  r <- f h
  case r of
   Nothing -> mapMaybeM f t
   Just x -> (x :) <$> mapMaybeM f t

validPaths :: Field -> PathTree
validPaths (Field { unit = Nothing }) = error "validPaths: no unit on the field"
validPaths startf@(Field { unit = Just startu }) = evalState (myPath startf) S.empty
  where path :: [Command] -> (Field -> State OldsSet PathTree) -> Field -> State OldsSet PathTree
        path _ _ (Field { unit = Nothing }) = error "path: no unit on the field"
        path cmds next f@(Field { unit = Just u }) = do
          modify $ S.insert $ absCoords u
          Crossroad <$> M.fromList <$> mapMaybeM (\c -> fmap (c, ) <$> check (command c f)) cmds

          where 
                dead f' = Just $ DeadEnd (absCoords u) (score f' - startScore)

                check Nothing = return $ Just $ DeadEnd S.empty 0
                check (Just f'@(Field { unit = Just u' })) = do
                  olds <- get
                  if | sourceLength f' /= sourceLength startf -> return $ dead f'
                     | absCoords u' `S.member` olds -> return Nothing
                     | otherwise -> Just <$> next f'
                check (Just f'@(Field { unit = Nothing })) = return $ dead f'

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
  where sols cmds (DeadEnd cells scr) = M.singleton cells (reverse cmds, scr)
        sols cmds (Crossroad ts) = foldr M.union M.empty $ map (\(c, t) -> sols (c:cmds) t) $ M.toList ts

findBest :: Field -> Solutions -> (Solution, Float)
findBest field = maximumBy (comparing snd) . map (\(cells, (sol, int)) -> (sol, scorify cells sol int)) . M.toList
  where scorify cells sol scor = a1 * scor + 
          a2 * low cells + 
          a3 * whole +
          a4 * bump

        bumpiness (a:b:hs) = abs (a - b) + bumpiness hs
        bumpiness _ = 0

        cols' = M.fromListWith min $ map ((\(V2 x y) -> (x, y)) . hcellToCell) $ S.toList $ filled field
        cols = map snd $ M.toAscList $ cols' `M.union` M.fromList (zip [0..width field - 1] [0,0..])

        low cells = fromIntegral $ sum $ map (\(V3 _ _ z) -> z) $ S.toList cells
        whole = sum (map (\c -> if S.null $ neighbors c S.\\ filled field then 1 else 0) $ S.toList $ filled field)
        bump = fromIntegral $ bumpiness cols

        a1 = 0.02
        a2 = 1.0
        a3 = 2.0
        a4 = -5.0

data Bot = Bot { solution :: !Solution
               , unitNum :: !Int
               }
         deriving (Show, Eq)

newBot :: Field -> Bot
newBot f = Bot { solution = fst $ findBest f $ solutions $ validPaths f
               , unitNum = sourceLength f
               }

advanceBot :: Field -> Bot -> (Command, Bot)
advanceBot f bot = (c, bot' { solution = cmds })
  where bot'@(Bot { solution = c:cmds })
          | sourceLength f /= unitNum bot = Bot { solution = fst $ findBest f $ solutions $ validPaths f
                                               , unitNum = sourceLength f
                                               }
          | otherwise = bot
