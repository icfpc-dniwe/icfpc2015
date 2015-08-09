{-# LANGUAGE TupleSections #-}

module Bot where

import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

import Types
import Field

data SolTree = DeadEnd
             | Crossroad (Map Command SolTree)
             deriving (Show, Eq)

validSolutionsSimple :: Field -> SolTree
validSolutionsSimple = path S.empty
  where path _ (Field { unit = Nothing }) = DeadEnd
        path olds f@(Field { unit = Just u }) = Crossroad $ M.fromList $ mapMaybe (\c -> (c, ) <$> (command c f >>= next)) [minBound..maxBound]
          where olds' = S.insert (absCoords u) olds
                next f'@(Field { unit = Just u' })
                  | absCoords u' `S.member` olds' = Nothing
                  | otherwise = Just $ path olds' f'
                next (Field { unit = Nothing }) = Just DeadEnd
