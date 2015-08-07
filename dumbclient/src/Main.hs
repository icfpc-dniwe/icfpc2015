import Graphics.Gloss

import Data (getInput)
import Field
import Visualize

main :: IO ()
main = do
  inp <- getInput 2
  let pic = fieldPicture $ result $ toField inp
  display (InWindow "Visualizer" (1024, 768) (0, 0)) black pic
