{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Data.Maybe
import System.IO
import Control.Concurrent (setNumCapabilities)
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
                           , coresNumber :: Int
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
  <$> some (strOption
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
  <> help "Available processor cores"
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

data PlayState = PlayState { rawInput :: !D.RawInput
                           , commands :: !Solution
                           , bot :: !Bot
                           , field :: !Field
                           , leftFields :: ![Field]
                           , finishedFields :: ![Solution]
                           , nextProblem :: !(IO (Maybe PlayState))
                           }

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

processState :: Command -> Field -> IO Field
processState cmd f = do
  let field' = command cmd f
  hPutStrLn stderr $
    "Score: " ++ show (score field')
    ++ ", left " ++ show (sourceLength field')
    ++ ", last command: " ++ show cmd
  when (isNothing $ unit field') $ hPutStrLn stderr "Finished placement"
  return field'

playShow :: VisState -> IO Picture
playShow s = return $ applyViewPortToPicture (viewStateViewPort $ viewState s) $ pic s

runBot :: Arguments -> PlayState -> IO ()
runBot args s = do
  let (cmd, bot') = advanceBot (field s) (bot s)
  field' <- processState cmd $ field s
  let cmds' = cmd : commands s
  if isNothing $ unit field'
    then finish (reverse cmds')
    else runBot args s { field = field'
                       , commands = cmds'
                       , bot = bot'
                       }
  where finish cmds' = if null (leftFields s)
                       then do
                         output args (rawInput s) $ finishedFields s ++ [cmds']
                         next <- nextProblem s
                         case next of
                          Just ps' -> runBot args ps'
                          Nothing -> return ()
                       else runBot args s { commands = []
                                          , field = head $ leftFields s
                                          , bot = newBot $ head $ leftFields s
                                          , finishedFields = finishedFields s ++ [cmds']
                                          , leftFields = tail $ leftFields s
                                          }

playEvent :: Arguments -> Event -> VisState -> IO VisState
playEvent args ev@(EventKey c Down _ _) s = case c of
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
          field' <- processState cmd $ field $ playState s
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
                         output args (rawInput $ playState s) $ finishedFields (playState s) ++ [cmds']
                         next <- nextProblem $ playState s
                         case next of
                          Just ps' -> return s { playState = ps'
                                              , pic = fieldPicture $ resultField $ field ps'
                                              }
                          Nothing -> fail "Finished!"
                       else let ps' = (playState s) { commands = []
                                                    , field = head $ leftFields $ playState s
                                                    , bot = newBot $ head $ leftFields $ playState s
                                                    , finishedFields = finishedFields (playState s) ++ [cmds']
                                                    , leftFields = tail $ leftFields $ playState s
                                                    }
                            in return s { playState = ps'
                                        , pic = fieldPicture $ resultField $ field ps'
                                        }

playEvent _ ev s = return s { viewState = updateViewStateWithEvent ev $ viewState s }

visEvent :: Event -> VisState -> IO VisState
visEvent (EventKey (SpecialKey KeySpace) Down _ _) s = do
  case commands $ playState s of
   [] -> do
     hPutStrLn stderr "No more commands"
     finish
   (cmd:cmds) -> do
     field' <- processState cmd $ field $ playState s
     if isNothing $ unit field'
       then finish
       else return s { playState = (playState s) { field = field'
                                                 , commands = cmds
                                                 }
                     , pic = fieldPicture $ resultField field'
                     }

  where finish = if null (leftFields $ playState s)
                 then do
                   next <- nextProblem $ playState s
                   case next of
                       Just ps' -> return s { playState = ps'
                                           , pic = fieldPicture $ resultField $ field ps'
                                           }
                       Nothing -> fail "Finished!"
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

  when (coresNumber args /= 0) $ setNumCapabilities $ coresNumber args

  case inputFormat args of
   Standard -> do
     inps <- mapM getRInput (filePaths args)
     let fill [] = return Nothing
         fill (i:is) = return $ Just PlayState { rawInput = i
                                               , commands = []
                                               , bot = newBot startField
                                               , field = startField
                                               , leftFields = map (toField i) [1..length (D.sourceSeeds i) - 1]
                                               , finishedFields = []
                                               , nextProblem = fill is
                                               }
           where startField = toField i 0

     Just ps <- fill inps
     let startState = VisState { playState = ps
                               , viewState = viewStateInit
                               , pic = fieldPicture $ resultField $ field ps
                               }
     if visualize args
       then playIO window black 30 startState playShow (playEvent args) playAdvance
       else runBot args ps

   Solved -> do
     inps <- mapM getFile (filePaths args)
     let fill [] = return Nothing
         fill (i:is) = do
           problem <- getInput $ problemId i
           let startField = toField problem 0
           return $ Just PlayState { rawInput = problem
                                   , commands = dewordify $ solution i
                                   , bot = newBot startField
                                   , field = startField
                                   , leftFields = map (toField problem) [1..length (D.sourceSeeds problem) - 1]
                                   , finishedFields = []
                                   , nextProblem = fill is
                                   }

     Just ps <- fill inps
     let startState = VisState { playState = ps
                               , viewState = viewStateInit
                               , pic = fieldPicture $ resultField $ field ps
                               }
     if visualize args
       then playIO window black 30 startState playShow visEvent playAdvance
       else fail "Non-interactive Solved is bot unsupported"

main :: IO ()
main = execParser opts >>= process
  where
    opts = info (helper <*> arguments)
      (  fullDesc
      <> progDesc "ICFPC 2015 bot and visualizer."
      )
