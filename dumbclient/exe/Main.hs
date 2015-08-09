{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Control.Exception
import Data.Char
import Data.List
import Data.Maybe
import System.IO
import qualified Data.Aeson as J
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import Options.Applicative hiding (command)
import Graphics.Gloss.Interface.Pure.Display
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Data.ViewState hiding (Command)
import System.Environment

import Bot hiding (solution)
import Types
import qualified ReadWrite as D
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
                 | Solved
                 deriving (Show, Read, Eq)

data Arguments = Arguments { inputType :: InputType
                           , inputFormat :: InputFormat
                           , onlineProblem :: Integer
                           , filePath :: String
                           , seedNo :: Int
                           , ourTag :: String
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
  <*> strOption
  (  short 'g'
  <> long "tag"
  <> help "Tag"
  <> value "Vis"
  )

data PlayState = PlayState { commands :: Solution
                           , bot :: Bot
                           , viewState :: ViewState
                           , field :: Field
                           , pic :: Picture
                           }

playShow :: PlayState -> IO Picture
playShow s = return $ applyViewPortToPicture (viewStateViewPort $ viewState s) $ pic s

playEvent :: D.RawInput -> Arguments -> Event -> PlayState -> IO PlayState
playEvent ri args ev@(EventKey c Down _ _) s = case c of
  Char 'h' -> doCmd' (Move W) >>= updBot
  Char 'j' -> doCmd' (Move SW) >>= updBot
  Char 'k' -> doCmd' (Move SE) >>= updBot
  Char 'l' -> doCmd' (Move E) >>= updBot
  Char 'u' -> doCmd' (Turn CW) >>= updBot
  Char 'i' -> doCmd' (Turn CCW) >>= updBot
  SpecialKey KeySpace -> do
    let (cmd, bot') = advanceBot (field s) (bot s)
    s' <- doCmd' cmd
    return $ s' { bot = bot' }
  _ -> return s { viewState = updateViewStateWithEvent ev $ viewState s }

  where updBot ss = return ss { bot = newBot (field ss) }
        doCmd' cmd = do
          field' <- case command cmd $ field s of
                    Nothing -> do
                      putStrLn "Placement failure!"
                      finish (reverse $ commands s)
                    Just f -> return f
          print (score field', sourceLength field', cmd)
          let cmds' = cmd : commands s
          when (isNothing $ unit field') $ do
            putStrLn "Success!"
            finish (reverse cmds')
          return s { field = field'
                   , commands = cmds'
                   , pic = fieldPicture $ resultField field'
                   }

        finish cmds' = do
          print cmds'
          let cmdsS = wordify cmds'
              output = RawOutput { problemId = onlineProblem args
                                 , seed = D.sourceSeeds ri !! seedNo args
                                 , tag = T.pack $ ourTag args
                                 , solution = cmdsS
                                 }
          BL.putStrLn $ J.encode output
          case inputType args of
           File -> return ()
           Online -> do
             send <- queryUser "Send solution to the server?" False
             when send $ do
               team <- read <$> getEnv "TEAM_ID"
               token <- B.pack <$> getEnv "TOKEN"
               postOutput team token [output]
          fail "Finished!"
  
playEvent _ _ ev s = return s { viewState = updateViewStateWithEvent ev $ viewState s }

visEvent :: Event -> PlayState -> IO PlayState
visEvent (EventKey (SpecialKey KeySpace) Down _ _) s@(PlayState { commands = cmd:cmds }) = do
  field' <- case command cmd $ field s of
    Nothing -> fail "Placement failure!"
    Just f -> return f
  print (score field', sourceLength field', cmd)
  when (isNothing $ unit field') $ do
    fail "Success!"
  return s { field = field'
           , commands = cmds
           , pic = fieldPicture $ resultField field'
           }
visEvent _ (PlayState { commands = [] }) = fail "No more commands"
visEvent ev s = return s { viewState = updateViewStateWithEvent ev $ viewState s }

playAdvance :: Float -> PlayState -> IO PlayState
playAdvance _ = return

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
          startState = PlayState { commands = []
                                 , viewState = viewStateInit
                                 , field = startField
                                 , bot = newBot startField
                                 , pic = fieldPicture $ resultField startField
                                 }

      playIO window black 30 startState playShow (playEvent inp args) playAdvance

    (File, Processed) -> do
      inp <- getFile
      let pict = fieldPicture $ processedField inp
      display window black pict

    (File, Solved) -> do
      inp' <- getFile
      let inp = inp' !! seedNo args
      problem <- getInput $ problemId inp
      let startField = toField problem (fromJust $ findIndex (== seed inp) (D.sourceSeeds problem))
          startState = PlayState { commands = dewordify $ solution inp
                                 , viewState = viewStateInit
                                 , field = startField
                                 , bot = error "bot undefined for solution visualizations"
                                 , pic = fieldPicture $ resultField startField
                                 }

      playIO window black 30 startState playShow visEvent playAdvance

    _ -> fail "Incompatible combination of input and format"

main :: IO ()
main = execParser opts >>= visualize
  where
    opts = info (helper <*> arguments)
      (  fullDesc
      <> progDesc "Visualize ICFPC 2015 maps: HJKL to move, UI to rotate."
      )
