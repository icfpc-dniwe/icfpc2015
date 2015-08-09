{-# LANGUAGE TupleSections #-}

module Field where

import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Map as M
import Linear.V2
import Linear.V3

import qualified ReadWrite as D
import LCG
import Types

type HCell = V3 Int

cellToHCell :: Cell -> HCell
cellToHCell (V2 x y) = V3 xx (-xx - y) y
  where xx = x - (y - y `mod` 2) `div` 2

hcellToCell :: HCell -> Cell
hcellToCell (V3 x _ z) = V2 (x + (z - z `mod` 2) `div` 2) z

type HUnit = Set HCell

data Field = Field { filled :: Set HCell
                   , width :: Int
                   , height :: Int
                   , unitCenter :: HCell
                   , unit :: HUnit
                   , availableUnits :: [HUnit]
                   , sourceLength :: Int
                   , randGen :: LCG
                   , score :: Float
                   , prevLines :: Int
                   }
           deriving (Show, Eq)

-- TODO: more things in cubic coordinates

validate :: Field -> Bool
validate f = distinct && borders
  where cells = S.map (+ unitCenter f) $ unit f
        distinct = cells == (cells S.\\ filled f)
        borders = all (\(V2 x y) -> x >= 0 && x < width f && y >= 0 && y < height f) $ map hcellToCell $ S.toList cells

validate' :: Field -> Maybe Field
validate' f = if validate f then Just f else Nothing

unitPlace :: HUnit -> Int -> HCell
unitPlace u w = cellToHCell $ V2 (c - left) (-top)
  where border = map hcellToCell $ S.toList u
        left = minimum $ map (\(V2 x _) -> x) border
        right = maximum $ map (\(V2 x _) -> x) border
        top = minimum $ map (\(V2 _ y) -> y) border
        c = ((w - (right - left)) `div` 2) - 1

clearLines :: Field -> (Int, Field)
clearLines f = (tn, f { filled = S.fromList $ concatMap revert $ M.toList tm })

  where (tn, tm) = foldr (clear . hcellToCell) (0, M.empty) $ S.toList $ filled f

        revert (y, xs) = map (cellToHCell . (\x -> V2 x y)) $ S.toList xs
        clear (V2 x y) (n, m)
          | S.size res == width f = (n + 1, M.fromList $ mapMaybe removeRed $ M.toList m)
          | otherwise = (n, M.insert y res m)
          where res = S.insert x $ M.findWithDefault S.empty y m
                removeRed (y', xs)
                  | y' == y = Nothing
                  | y' < y = Just (y' + 1, xs)
                  | otherwise = Just (y', xs)

moveScore :: HUnit -> Int -> Int -> Float
moveScore u curr prev = points + bonus
  where points = fromIntegral (S.size u) + 100 * (1 + fromIntegral curr) * fromIntegral curr / 2
        bonus
          | prev > 1 = fromIntegral (floor $ (fromIntegral prev - 1) * points / 10 :: Int)
          | otherwise = 0

nextUnit :: Field -> Maybe (Field, Bool)
nextUnit f = (, left <= 0) <$> validate' f'''

  where (un, gen) = nextLCG $ randGen f
        u = availableUnits f !! (fromIntegral un `mod` length (availableUnits f))
        left = sourceLength f - 1

        f' = f { filled = filled f `S.union` S.map (+ unitCenter f) (unit f)
               , unit = u
               , unitCenter = unitPlace u (width f)
               , sourceLength = left
               , randGen = gen
               }

        (lnum, f'') = clearLines f'

        f''' = f'' { score = moveScore (unit f'') lnum (prevLines f'') + score f''
                   , prevLines = lnum
                   }

toHUnit :: D.Unit -> HUnit
toHUnit u = S.map ((subtract pivot) . cellToHCell) $ D.members u
  where pivot = cellToHCell $ D.pivot u

toField :: D.RawInput -> Int -> Field
toField input nseed = (fst $ fromJust $ nextUnit f) { score = 0 }
  where f = Field { filled = S.map cellToHCell $ D.filled input
                  , width = D.width input
                  , height = D.height input
                  , unitCenter = V3 0 0 0
                  , unit = S.empty
                  , availableUnits = map toHUnit $ D.units input
                  , sourceLength = D.sourceLength input
                  , randGen = defLCG $ D.sourceSeeds input !! nseed
                  , score = 0
                  , prevLines = 0
                  }

resultField :: Field -> Visualized
resultField field = Visualized { visFilled = pts
                               , visWidth = width field
                               , visHeight = height field
                               }
  where builtCol = V3 0 1 0
        cellCol = V3 1 0 0
        built = M.fromList $ map ((, builtCol) . hcellToCell) $ S.toList $ filled field
        cells = M.fromList $ map convCells $ S.toList $ unit field
        convCells = (, cellCol) . (+ hcellToCell (unitCenter field)) . hcellToCell
        pts = built `M.union` cells

move :: Field -> Direction -> Maybe Field
move f d = validate' f { unitCenter = p + unitCenter f }
  where p = case d of
          E -> V3 1 (-1) 0
          W -> V3 (-1) 1 0
          SE -> V3 0 (-1) 1
          SW -> V3 (-1) 0 1

rotate :: Field -> TDirection -> Maybe Field
rotate f d = validate' f { unit = S.map conv $ unit f }
  where conv (V3 x y z) = case d of
          CW -> V3 (-z) (-x) (-y)
          CCW -> V3 (-y) (-z) (-x)

command :: Field -> Command -> Maybe Field
command f (Move m) = move f m
command f (Turn r) = rotate f r
