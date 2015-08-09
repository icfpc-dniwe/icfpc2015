{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Control.Exception
import Data.Char
import Data.Maybe
import System.IO
import qualified Data.Aeson as J
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Options.Applicative hiding (command)
import Graphics.Gloss.Interface.Pure.Display
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Data.ViewState hiding (Command)

import Types
import ReadWrite (RawOutput(..), getInput, postOutput)
import Field (Field(..), toField, command, resultField)
import ProcessedField (processedField)
import Visualize
import Wordify

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

type PlayState = ([Command], ViewState, Field, Picture)

playShow :: PlayState -> IO Picture
playShow (_, st, _, pic) = return $ applyViewPortToPicture (viewStateViewPort st) $ pic

playEvent :: Arguments -> Event -> PlayState -> IO PlayState
playEvent args ev@(EventKey (Char c) Down _ _) (cmds, st, field, pic) = case c of
  'h' -> doCmd' (Move W)
  'j' -> doCmd' (Move SW)
  'k' -> doCmd' (Move SE)
  'l' -> doCmd' (Move E)
  'u' -> doCmd' (Turn CW)
  'i' -> doCmd' (Turn CCW)
  _ -> return (cmds, updateViewStateWithEvent ev st, field, pic)

  where doCmd' cmd = do
          field' <- case command cmd field of
                    Nothing -> do
                      putStrLn "Placement failure!"
                      playFinish args cmds
                    Just f -> return f
          print (score field', sourceLength field', cmd)
          let cmds' = cmd : cmds
          when (isNothing $ unit field') $ do
            putStrLn "Success!"
            playFinish args (reverse cmds')
          return (cmds', st, field', fieldPicture $ resultField field')
  
playEvent _ ev (cmds, st, field, pic) = return (cmds, updateViewStateWithEvent ev st, field, pic)

playAdvance :: Float -> PlayState -> IO PlayState
playAdvance _ = return

playFinish :: Arguments -> [Command] -> IO a
playFinish args cmds = do
  print cmds
  let cmdsS = wordify cmds
  putStrLn cmdsS
  case inputType args of
   File -> return ()
   Online -> do
     send <- queryUser "Send solution to the server?" False
     when send $ postOutput team token [RawOutput { problemId = onlineProblem args
                                                  , seed = seedNo args
                                                  , tag = "Vis"
                                                  , solution = cmdsS
                                                  }
                                       ]
  fail "Finished!"

queryUser :: String -> Bool -> IO Bool
queryUser q def = bracket (hGetBuffering stdout) (hSetBuffering stdout) $ const $ do
  hSetBuffering stdout NoBuffering
  putStr $ q ++ " (" ++ (if def then "Y" else "y") ++ "/" ++ (if not def then "N" else "n") ++ "): "
  s <- getLine
  case map toLower s of
   "y" -> return True
   "n" -> return False
   _ -> return def

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
      inp <- case from of
        Online -> getInput $ onlineProblem args
        File -> getFile
      let startField = toField inp (seedNo args)
          startState = ([], viewStateInit, startField, fieldPicture $ resultField startField)

      playIO window black 30 startState playShow (playEvent args) playAdvance

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
      <> progDesc "Visualize ICFPC 2015 maps: HJKL to move, UI to rotate."
      )

token :: B.ByteString
token = "53/b8w5nkWTgqhWm00puFJMoBk3NPMMs3TAPAD8eSU0="

team :: Integer
team = 180
