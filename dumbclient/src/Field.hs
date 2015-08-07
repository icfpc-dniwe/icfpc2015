module Field where

import qualified Data as D
import Data (RawInput, Cell(..), Unit(..))

data Field = Field { filled :: [Cell]
                   , width :: Integer
                   , height :: Integer
                   , units :: [(Cell, Unit)]
                   }

rotate :: TDirection -> Unit -> Unit
rotate CW unit = unit { members = map (\c -> Cell { x = y c, y = x c }) members }
rotate CCW unit = unit { members = map (\c -> Cell { x = y c, y = negate $ x c }) members }

toField :: RawInput -> Field
toField input = Field { filled = D.filled input
                      , width = D.width input
                      , height = D.height input
                      , units = []
                      }
