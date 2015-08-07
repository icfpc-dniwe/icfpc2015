import Graphics.Gloss

import Data (getInput)
import Field
import Visualize
import System.Environment

main :: IO ()
main = do
  [n] <- getArgs
  inp <- getInput $ read n
  let pic = fieldPicture $ result $ toField inp
  display (InWindow "Visualizer" (1024, 768) (0, 0)) black pic
