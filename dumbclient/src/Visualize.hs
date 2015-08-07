module Visualize where

import Graphics.Gloss
import Data.Array

import Field

fieldPicture :: ResultField -> Picture
fieldPicture f = Scale 13 13 $ Pictures $ step False [0..height]
  where (_, (width, height)) = bounds f

        t = 2 * pi / 6
        pol = Polygon $ map (\n -> (sin (t*n), cos (t*n))) [1..6]
        cpol x y = Color clr pol
          where clr = case f ! (x, y) of
                  NotFilled -> white
                  Built -> green
                  CellPart -> red
        
        line y = map (\x -> Translate (fromIntegral x * 2) 0 $ cpol x y) [0..width]

        step _ [] = []
        step r (y:ys) = map (Translate (if r then 1 else 0) (negate $ fromIntegral y * 2)) (line y) ++ step (not r) ys
