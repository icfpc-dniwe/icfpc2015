{-# LANGUAGE TupleSections #-}

module Field where

import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Linear.V2
import Linear.V3
import Debug.Trace

import qualified ReadWrite as D
import LCG
import Types

type HCell = V3 Int

cellToHCell :: Cell -> HCell
cellToHCell (V2 x y) = V3 xx (-xx - y) y
  where xx = x - (y - y `mod` 2) `div` 2

hcellToCell :: HCell -> Cell
hcellToCell (V3 x _ z) = V2 (x + (z - z `mod` 2) `div` 2) z

type HCells = Set HCell

data HUnit = HUnit { center :: !HCell
                   , pivot :: !HCell
                   , members :: !HCells
                   }
           deriving (Show, Eq)

data Field = Field { filled :: !HCells
                   , width :: !Int
                   , height :: !Int
                   , unit :: !(Maybe HUnit)
                   , availableUnits :: ![HUnit]
                   , sourceLength :: !Int
                   , randGen :: !LCG
                   , score :: !Float
                   , prevLines :: !Int
                   }
           deriving (Show, Eq)

-- TODO: more things in cubic coordinates

absCoords :: HUnit -> HCells
absCoords u = S.map (+ center u) $ members u

validate :: Field -> Bool
validate f@(Field { unit = Just u }) = distinct && borders
  where cells = absCoords u
        distinct = cells == (cells S.\\ filled f)
        borders = all (\(V2 x y) -> x >= 0 && x < width f && y >= 0 && y < height f) $ map hcellToCell $ S.toList cells
validate (Field { unit = Nothing }) = True

unitPlace :: HUnit -> Int -> HCell
unitPlace u w = cellToHCell $ V2 (c - left) (-top)
  where border = map hcellToCell $ S.toList $ members u
        left = minimum $ map (\(V2 x _) -> x) border
        right = maximum $ map (\(V2 x _) -> x) border
        top = minimum $ map (\(V2 _ y) -> y) border
        c = (w - (right - left + 1)) `div` 2

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
  where points = fromIntegral (S.size $ members u) + 100 * (1 + fromIntegral curr) * fromIntegral curr / 2
        bonus
          | prev > 1 = fromIntegral (floor $ (fromIntegral prev - 1) * points / 10 :: Int)
          | otherwise = 0

freezeUnit :: Field -> Field
freezeUnit f@(Field { unit = Just u }) = f''
  where (lnum, f') = clearLines f { filled = filled f `S.union` S.map (+ center u) (members u) 
                                  , unit = Nothing
                                  }
        f'' = f' { score = moveScore u lnum (prevLines f') + score f'
                 , prevLines = lnum
                 }
freezeUnit f@(Field { unit = Nothing }) = f

nextUnit :: Field -> Maybe Field
nextUnit f
  | sourceLength f <= 0 = Just f
  | otherwise = if validate f' then Just f' else Nothing

  where (un, gen) = nextLCG $ randGen f
        u = availableUnits f !! (fromIntegral un `mod` length (availableUnits f))

        f' = f { unit = Just u { center = unitPlace u (width f)
                               }
               , sourceLength = sourceLength f - 1
               , randGen = gen
               }

toHUnit :: D.Unit -> HUnit
toHUnit u = hu
  where cpivot = cellToHCell $ D.pivot u
        hu = HUnit { center = V3 0 0 0
                   , pivot = cpivot
                   , members = S.map cellToHCell $ D.members u
                   }

toField :: D.RawInput -> Int -> Field
toField input nseed = fromJust $ nextUnit f
  where f = Field { filled = S.map cellToHCell $ D.filled input
                  , width = D.width input
                  , height = D.height input
                  , unit = Nothing
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
        cells = case unit field of
                 Just u -> M.fromList $ map (convCell $ center u) $ S.toList $ members u
                 Nothing -> M.empty
        convCell cc = (, cellCol) . hcellToCell . (+ cc)
        pts = built `M.union` cells

move :: HUnit -> Direction -> HUnit
move u d = u { center = p + center u }
  where p = case d of
          E -> V3 1 (-1) 0
          W -> V3 (-1) 1 0
          SE -> V3 0 (-1) 1
          SW -> V3 (-1) 0 1

rotate :: HUnit -> TDirection -> HUnit
rotate u d = u { members = S.map ((+ pivot u) . conv . subtract (pivot u)) $ members u }
  where conv (V3 x y z) = case d of
          CW -> V3 (-z) (-x) (-y)
          CCW -> V3 (-y) (-z) (-x)

command :: Command -> Field -> Maybe Field
command cmd f@(Field { unit = Just u })
  | validate f' = Just f'
  | otherwise = case nextUnit (freezeUnit f) of
    Nothing -> Nothing
    Just f'' -> Just f''--command cmd f''

  where f' = f { unit = Just $ case cmd of
                  Move m -> move u m
                  Turn r -> rotate u r
               }
command _ f@(Field { unit = Nothing }) = Just f


neighbors :: HCell -> Set HCell
neighbors c = S.fromList [c + sNW, c + sNE, c + sE, c + sSE, c + sSW, c + sW]
  where sNW = V3 0 1 (-1)
        sNE = V3 1 0 (-1)
        sE  = V3 1 (-1) 0
        sSE = V3 0 (-1) 1
        sSW = V3 (-1) 0 1
        sW  = V3 (-1) 1 0
