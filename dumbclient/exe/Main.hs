{-# LANGUAGE DeriveDataTypeable, DeriveAnyClass #-}

import Control.Exception
import Control.Monad
import Data.Typeable
import Data.IORef
import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy as BL
import Options.Applicative hiding (command)
import Graphics.Gloss.Interface.Pure.Display
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Data.ViewState hiding (Command)

import Types
import ReadWrite (getInput)
import Field (Field(..), toField, nextUnit, command, resultField)
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
                           , seedNo :: Int
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
  <*> option auto
  (  short 's'
  <> long "seed"
  <> help "Seed number"
  <> value 0
  )

type PlayState = (ViewState, Field, Picture)

playShow :: PlayState -> IO Picture
playShow (st, _, pic) = return $ applyViewPortToPicture (viewStateViewPort st) $ pic

data PlacementFailure = PlacementFailure
                      deriving (Show, Typeable, Exception)

data FinishedPlaying = FinishedPlaying
                     deriving (Show, Typeable, Exception)

doCmd :: Command -> Field -> (Field, Bool)
doCmd c field = case command field c of
  Nothing -> case nextUnit field of
    Nothing -> throw PlacementFailure
    Just ub -> ub
  Just u -> (u, False)

playEvent :: IORef [Command] -> Event -> PlayState -> IO PlayState
playEvent cmds ev@(EventKey (Char c) Down _ _) (st, field, pic) = case c of
  'h' -> doCmd' (Move W)
  'j' -> doCmd' (Move SW)
  'k' -> doCmd' (Move SE)
  'l' -> doCmd' (Move E)
  'u' -> doCmd' (Turn CW)
  'i' -> doCmd' (Turn CCW)
  _ -> return (updateViewStateWithEvent ev st, field, pic)

  where doCmd' cmd = do
          let (field', b) = doCmd cmd field
          print (score field', sourceLength field', c)
          modifyIORef cmds (cmd:)
          when b $ throwIO FinishedPlaying
          return (st, field', fieldPicture $ resultField field')
  
playEvent _ ev (st, field, pic) = return (updateViewStateWithEvent ev st, field, pic)

playAdvance :: Float -> PlayState -> IO PlayState
playAdvance _ = return

visualize :: Arguments -> IO ()
visualize args = do
  let window = InWindow "Visualizer" (1024, 768) (0, 0)
      getFile :: J.FromJSON a => IO a
      getFile = do
        s <- case filePath args of
          "-" -> BL.getContents
          fp -> BL.readFile fp
        case J.decode s of
         Nothing -> fail "Failed to decode JSON from file"
         Just i -> return i

  case (inputType args, inputFormat args) of
    (from, Standard) -> do
      path <- newIORef []
      inp <- case from of
        Online -> getInput $ onlineProblem args
        File -> getFile
      let startField = toField inp (seedNo args)
          startState = (viewStateInit, startField, fieldPicture $ resultField startField)

      playIO window black 30 startState playShow (playEvent path) playAdvance
        `catch`
        (\FinishedPlaying -> do
            cmds <- reverse <$> readIORef path
            print cmds
        )

    (File, Processed) -> do
      inp <- getFile
      let pic = fieldPicture $ processedField inp
      display window black pic
    _ -> fail "Incompatible combination of input and format"

main :: IO ()
main = execParser opts >>= visualize
  where
    opts = info (helper <*> arguments)
      (  fullDesc
      <> progDesc "Visualize ICFPC 2015 maps"
      )
