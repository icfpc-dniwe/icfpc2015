module Bot where

import Data.List

import Types
import Field

validSolutions :: Field -> [Solution]
validSolutions f@(Field { unit = Just u }) = undefined
validSolutions (Field { unit = Nothing }) = []
