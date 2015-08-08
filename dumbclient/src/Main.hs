import Graphics.Gloss

import Data.Maybe
import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy as BL
import Options.Applicative

import ReadWrite (getInput)
import Field (toField, resultField)
import ProcessedField (processedField)
import Visualize

data InputType = Online
               | File
               deriving (Show, Read, Eq)

data InputFormat = Standard
                 | Processed
                 deriving (Show, Read, Eq)

data Arguments = Arguments { inputType :: InputType
                           , inputFormat :: InputFormat
                           , onlineProblem :: Integer
                           , filePath :: String
                           }
               deriving (Show, Eq)

arguments :: Parser Arguments
arguments = Arguments
  <$> option auto
  (  short 't'
  <> long "type"
  <> help "Where to take input from"
  <> value Online
  )
  <*> option auto
  (  short 'f'
  <> long "format"
  <> help "Input format"
  <> value Standard
  )
  <*> option auto
  (  short 'p'
  <> long "problem"
  <> help "Problem number"
  <> value 0
  )
  <*> strOption
  (  short 'i'
  <> long "input"
  <> help "File with input"
  <> value ""
  )

visualize :: Arguments -> IO ()
visualize args = do
  res <- case (inputType args, inputFormat args) of
    (Online, Standard) -> do
      inp <- getInput $ onlineProblem args
      return $ resultField $ toField inp 0
    (File, fmt) -> do
      f <- case filePath args of
        "-" -> BL.getContents
        fp -> BL.readFile fp
      case fmt of
       Standard -> do
         let inp = fromJust $ J.decode f
         return $ resultField $ toField inp 0
       Processed -> do
         let inp = fromJust $ J.decode f
         return $ processedField inp
  let pic = fieldPicture res
  display (InWindow "Visualizer" (1024, 768) (0, 0)) black pic

main :: IO ()
main = execParser opts >>= visualize
  where
    opts = info (helper <*> arguments)
      (  fullDesc
      <> progDesc "Visualize ICFPC 2015 maps"
      )
