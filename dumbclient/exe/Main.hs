{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
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
import Visualize
import Wordify

data InputSource = Online
                 | File
                 deriving (Show, Read, Eq)

data InputFormat = Standard
                 | Solved
                 deriving (Show, Read, Eq)

data OutputMode = Send
                | Print
                deriving (Show, Read, Eq)

data Arguments = Arguments { filePaths :: [String]
                           , timeLimit :: Integer
                           , memoryLimit :: Integer
                           , coresNumber :: Integer
                           , phrases :: [String]
                           , inputSource :: InputSource
                           , inputFormat :: InputFormat
                           , outputMode :: OutputMode
                           , outputTag :: String
                           , visualize :: Bool
                           }
               deriving (Show, Eq)

arguments :: Parser Arguments
arguments = Arguments
  <$> many (strOption
  (  short 'f'
  <> long "file"
  <> metavar "FILENAME"
  <> help "Input files"
  ))
  <*> option auto
  (  short 't'
  <> long "time"
  <> metavar "NUMBER"
  <> help "Time limit (0 - no limit)"
  <> value 0
  <> showDefault
  )
  <*> option auto
  (  short 'm'
  <> long "memory"
  <> metavar "NUMBER"
  <> help "Memory limit (0 - no limit)"
  <> value 0
  <> showDefault
  )
  <*> option auto
  (  short 'c'
  <> long "cores"
  <> metavar "NUMBER"
  <> help "Available processor cores (0 - autodetect)"
  <> value 0
  <> showDefault
  )
  <*> many (strOption
  (  short 'p'
  <> long "phrase"
  <> metavar "STRING"
  <> help "Phrase of power"
  ))
  <*> option auto
  (  short 's'
  <> long "source"
  <> help "Input source"
  <> value File
  <> showDefault
  )
  <*> option auto
  (  short 'F'
  <> long "format"
  <> help "Input format"
  <> value Standard
  <> showDefault
  )
  <*> option auto
  (  short 'o'
  <> long "output"
  <> help "Output mode"
  <> value Print
  <> showDefault
  )
  <*> strOption
  (  short 'T'
  <> long "tag"
  <> help "Tag"
  <> value "dumbclient"
  <> showDefault
  )
  <*> switch
  (  short 'v'
  <> long "visualize"
  <> help "Graphical mode: HJKL to move, UI to rotate"
  )

data PlayState = PlayState { commands :: !Solution
                           , bot :: !Bot
                           , field :: !Field
                           , leftFields :: ![Field]
                           , finishedFields :: ![Solution]
                           }
               deriving (Show, Eq)

data VisState = VisState { playState :: !PlayState
                         , viewState :: !ViewState
                         , pic :: !Picture
                         }

output :: Arguments -> D.RawInput -> [Solution] -> IO ()
output args inp outps = do
  let conv (n, outp) = RawOutput { problemId = D.id inp
                                 , seed = D.sourceSeeds inp !! n
                                 , tag = T.pack $ outputTag args
                                 , solution = wordify outp
                                 }
      outps' = map conv $ zip [0..] outps
  case outputMode args of
   Print -> BL.putStrLn $ J.encode outps'
   Send -> do
     BL.hPutStrLn stderr $ J.encode outps'
     team <- read <$> getEnv "TEAM_ID"
     token <- B.pack <$> getEnv "TOKEN"
     postOutput team token outps'

processState :: Command -> Field -> IO (Maybe Field)
processState cmd f = case command cmd f of
  Nothing -> do
    hPutStrLn stderr "Placement failure!"
    return Nothing
  Just field' -> do
    hPutStrLn stderr $
      "Score: " ++ show (score field')
      ++ ", left " ++ show (sourceLength field')
      ++ ", last command: " ++ show cmd
    when (isNothing $ unit field') $ hPutStrLn stderr "Success!"
    return $ Just field'

playShow :: VisState -> IO Picture
playShow s = return $ applyViewPortToPicture (viewStateViewPort $ viewState s) $ pic s

runBot :: D.RawInput -> Arguments -> PlayState -> IO ()
runBot ri args s = do
  let (cmd, bot') = advanceBot (field s) (bot s)
  mfield <- processState cmd $ field s
  case mfield of
   Nothing -> finish (reverse $ commands s)
   Just field' -> do
     let cmds' = cmd : commands s
     if isNothing $ unit field'
       then finish (reverse cmds')
       else runBot ri args s { field = field'
                             , commands = cmds'
                             , bot = bot'
                             }
  where finish cmds' = if null (leftFields s)
                       then output args ri $ finishedFields s ++ [cmds']
                       else runBot ri args s { commands = []
                                             , field = head $ leftFields s
                                             , bot = newBot $ head $ leftFields s
                                             , finishedFields = finishedFields s ++ [cmds']
                                             , leftFields = tail $ leftFields s
                                             }

playEvent :: D.RawInput -> Arguments -> Event -> VisState -> IO VisState
playEvent ri args ev@(EventKey c Down _ _) s = case c of
  Char 'h' -> doCmd (Move W) >>= updBot
  Char 'j' -> doCmd (Move SW) >>= updBot
  Char 'k' -> doCmd (Move SE) >>= updBot
  Char 'l' -> doCmd (Move E) >>= updBot
  Char 'u' -> doCmd (Turn CW) >>= updBot
  Char 'i' -> doCmd (Turn CCW) >>= updBot
  SpecialKey KeyEsc -> finish $ reverse $ commands $ playState s
  SpecialKey KeySpace -> do
    let (cmd, bot') = advanceBot (field $ playState s) (bot $ playState s)
    s' <- doCmd cmd
    return s' { playState = (playState s') { bot = bot' } }
  _ -> return s { viewState = updateViewStateWithEvent ev $ viewState s }

  where updBot ss = return ss { playState = (playState ss) { bot = newBot (field $ playState ss) } }

        doCmd cmd = do
          mfield <- processState cmd $ field $ playState s
          case mfield of
           Nothing -> do
             finish $ reverse $ commands $ playState s
           Just field' -> do
             let cmds' = cmd : commands (playState s)
             if isNothing $ unit field'
               then finish (reverse cmds')
               else return s { playState = (playState s) { field = field'
                                                         , commands = cmds'
                                                         }
                             , pic = fieldPicture $ resultField field'
                             }
        
        finish cmds' = if null (leftFields $ playState s)
                       then do
                         output args ri $ finishedFields (playState s) ++ [cmds']
                         fail "Finished!"
                       else let ps' = (playState s) { commands = []
                                                    , field = head $ leftFields $ playState s
                                                    , bot = newBot $ head $ leftFields $ playState s
                                                    , finishedFields = finishedFields (playState s) ++ [cmds']
                                                    , leftFields = tail $ leftFields $ playState s
                                                    }
                            in return s { playState = ps'
                                        , pic = fieldPicture $ resultField $ field ps'
                                        }

playEvent _ _ ev s = return s { viewState = updateViewStateWithEvent ev $ viewState s }

visEvent :: Event -> VisState -> IO VisState
visEvent (EventKey (SpecialKey KeySpace) Down _ _) s = do
  case commands $ playState s of
   [] -> do
     hPutStrLn stderr "No more commands"
     finish
   (cmd:cmds) -> do
     mfield <- processState cmd $ field $ playState s
     case mfield of
      Nothing -> do
        hPutStrLn stderr "Placement failure!"
        finish
      Just field' -> do
        if isNothing $ unit field'
          then finish
          else return s { playState = (playState s) { field = field'
                                                    , commands = cmds
                                                    }
                        , pic = fieldPicture $ resultField field'
                        }

  where finish = if null (leftFields $ playState s)
                 then fail "Finished!"
                 else let ps' = (playState s) { commands = head $ finishedFields $ playState s
                                              , field = head $ leftFields $ playState s
                                              , bot = newBot $ head $ leftFields $ playState s
                                              , finishedFields = tail $ finishedFields $ playState s
                                              , leftFields = tail $ leftFields $ playState s
                                              }
                      in return s { playState = ps'
                                  , pic = fieldPicture $ resultField $ field ps'
                                  }

visEvent ev s = return s { viewState = updateViewStateWithEvent ev $ viewState s }

playAdvance :: Float -> VisState -> IO VisState
playAdvance _ = return

process :: Arguments -> IO ()
process args = do
  let window = InWindow "Visualizer" (1024, 768) (0, 0)

      getFile :: J.FromJSON a => String -> IO a
      getFile fp = do
        s <- BL.readFile fp
        case J.decode s of
         Nothing -> fail "Failed to decode JSON from file"
         Just r -> return r

      getRInput :: String -> IO D.RawInput
      getRInput fp = case inputSource args of
        Online -> getInput $ read fp
        File -> getFile fp

  case inputFormat args of
   Standard -> do
     -- TODO: fix
     [inp] <- mapM getRInput (filePaths args)
     let startField = toField inp 0
         startPState = PlayState { commands = []
                                 , bot = newBot startField
                                 , field = startField
                                 , leftFields = map (toField inp) [1..length (D.sourceSeeds inp) - 1]
                                 , finishedFields = []
                                 }
         startState = VisState { playState = startPState
                               , viewState = viewStateInit
                               , pic = fieldPicture $ resultField startField
                               }
     if visualize args
       then playIO window black 30 startState playShow (playEvent inp args) playAdvance
       else runBot inp args startPState

   Solved -> do
     [inp] <- head <$> mapM getFile (filePaths args)
     problem <- getInput $ problemId inp
     let startField = toField problem 0
         startPState = PlayState { commands = dewordify $ solution inp
                                 , bot = newBot startField
                                 , field = startField
                                 , leftFields = map (toField problem) [1..length (D.sourceSeeds problem) - 1]
                                 , finishedFields = []
                                 }
         startState = VisState { playState = startPState
                               , viewState = viewStateInit
                               , pic = fieldPicture $ resultField startField
                               }
     if visualize args
       then playIO window black 30 startState playShow visEvent playAdvance
       else fail "Solved bot unsupported"

main :: IO ()
main = execParser opts >>= process
  where
    opts = info (helper <*> arguments)
      (  fullDesc
      <> progDesc "ICFPC 2015 bot and visualizer."
      )
