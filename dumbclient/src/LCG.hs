module LCG where

import Data.Bits

data LCG = LCG { seed :: Integer
               , modulus :: Integer
               , mult :: Integer
               , incr :: Integer
               }
           deriving (Show, Eq)

nextLCG :: LCG -> (Integer, LCG)
nextLCG g = (res, g { seed = seed' })
  where seed' = (mult g * seed g + incr g) `mod` modulus g
        res = (seed g `shiftR` 16) .&. 0xFFFF

defLCG :: Integer -> LCG
defLCG s = LCG { seed = s
               , modulus = 2 ^ (32 :: Integer)
               , mult = 1103515245
               , incr = 12345
               }
