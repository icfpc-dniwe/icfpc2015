module LCG where

import Data.Bits

data LCG = LCG { seed :: Integer
               , modulo :: Integer
               , mult :: Integer
               , incr :: Integer
               }
           deriving (Show, Eq)

nextLCG :: LCG -> (Integer, LCG)
nextLCG g = (res, g { seed = seed' })
  where seed' = (mult g * seed g + incr g) `mod` modulo g
        res = (seed g `shiftR` 16) .&. 0xFFFFFFFFFFFFFF

defLCG :: Integer -> LCG
defLCG s = LCG { seed = s
               , modulo = 2 ^ (32 :: Integer)
               , mult = 1103515245
               , incr = 12345
               }
