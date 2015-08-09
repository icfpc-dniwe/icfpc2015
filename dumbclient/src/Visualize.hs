module Visualize where

import qualified Data.Map as M
import Graphics.Gloss
import Linear.V2
import Linear.V3

import Types

fieldPicture :: Visualized -> Picture
fieldPicture f = Scale 13 13 $ Pictures $ step False [0..visHeight f - 1]
  where t = 2 * pi / 6
        pol = Polygon $ map (\n -> (sin (t*n), cos (t*n))) [1..6]
        cpol x y = Color (makeColor r g b 1) pol
          where V3 r g b = M.findWithDefault 1 (V2 x y) (visFilled f)
        pline y = map (\x -> Translate (fromIntegral x * 2) 0 $ cpol x y) [0..visWidth f - 1]

        step _ [] = []
        step r (y:ys) = map (Translate (if r then 1 else 0) (negate $ fromIntegral y * 2)) (pline y) ++ step (not r) ys
