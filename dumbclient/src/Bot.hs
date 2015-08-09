module Bot where

import Data.Map (Map)
import qualified Data.Map as M

import Types
import Field

data SolTree = DeadEnd
             | Crossroad (Map Command SolTree)
             deriving (Show, Eq)

--validSolutions :: Field -> SolTree
--validSolutions 

--validSolutions :: Field -> [Solution]
--validSolutions f@(Field { unit = Just u }) = do
--  let cws d = inits $ repeat (Rotate d)
--  rot <- take 3 (cws CW) ++ take 3 (cws CCW)
--  let move 
--validSolutions (Field { unit = Nothing }) = []
