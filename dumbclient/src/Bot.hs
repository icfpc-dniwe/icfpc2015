{-# LANGUAGE TupleSections #-}

module Bot where

import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Linear.V2

import Types
import Field

data SolTree = DeadEnd
             | Crossroad (Map Command SolTree)
             deriving (Show, Eq)

mergeSol :: SolTree -> SolTree -> SolTree
mergeSol DeadEnd DeadEnd = DeadEnd
mergeSol (Crossroad a) DeadEnd = Crossroad a
mergeSol DeadEnd (Crossroad b) = Crossroad b
mergeSol (Crossroad a) (Crossroad b) = Crossroad (a `M.union` b)

solLength :: SolTree -> Int
solLength DeadEnd = 1
solLength (Crossroad m) = sum $ map solLength $ M.elems m

type OldsSet = Set (Set HCell)

validSolutionsSimple :: Field -> SolTree
validSolutionsSimple (Field { unit = Nothing }) = DeadEnd
validSolutionsSimple startf@(Field { unit = Just startu }) = myPath S.empty startf
  where path :: [Command] -> (OldsSet -> Field -> SolTree) -> OldsSet -> Field -> SolTree
        path _ _ _ f | sourceLength f /= sourceLength startf = DeadEnd
        path _ _ _ (Field { unit = Nothing }) = DeadEnd
        path cmds next olds f@(Field { unit = Just u }) =
          Crossroad $ M.fromList $ mapMaybe (\c -> (c, ) <$> (command c f >>= check)) cmds

          where olds' = S.insert (absCoords u) olds
                check f'@(Field { unit = Just u' })
                  | absCoords u' `S.member` olds' = Nothing
                  | otherwise = Just $ next olds' f'
                check (Field { unit = Nothing }) = Just DeadEnd

        cheight = maximum ys - minimum ys
          where ys = map ((\(V2 _ y) -> y) . hcellToCell) $ S.toList $ members startu
        tryOr cmds next olds f = mergeSol (path cmds (tryOr cmds next) olds f) (next olds f)

        dropAll = path [Move SW, Move SE] dropAll
        dropSome 0 next = next
        dropSome n next = path [Move SW, Move SE] (dropSome (n - 1) next)
        turn = tryOr [Turn CW, Turn CCW]
        moveH = tryOr [Move W, Move E]

        myPath = moveH $ dropSome cheight $ turn $ dropAll
