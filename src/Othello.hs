{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

----------------------------------------------------------------------
-- Othello.hs
-- by Reed Rosenbluth
-- User interace module
----------------------------------------------------------------------

module Main where

import           Types
import           Game
import           AI

import           Control.Monad
import           Data.Array
import           Data.List.Split
import           System.Directory

import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core hiding (on)

----------------------------------------------------------------------
-- GUI
----------------------------------------------------------------------
main :: IO ()
main = do
  currentDirectory <- getCurrentDirectory
  let static = currentDirectory ++ "/static"
  startGUI defaultConfig { tpStatic = Just static } setup

getPieceUrl :: Piece -> FilePath
getPieceUrl Empty = "static/images/tile.png"
getPieceUrl Black = "static/images/black.png"
getPieceUrl White = "static/images/white.png"

-- List of images for initial Othello board configuration
initImgs :: [FilePath]
initImgs = replicate 27 (getPieceUrl Empty)
        ++ [getPieceUrl White] ++ [getPieceUrl Black]
        ++ replicate 6  (getPieceUrl Empty)
        ++ [getPieceUrl Black] ++ [getPieceUrl White]
        ++ replicate 27 (getPieceUrl Empty)

-- List of images for the current board
toUrls :: Game -> [FilePath]
toUrls (Game _ b) = [getPieceUrl $ b ! s | s <- squares]

showOpacity :: Bool -> [(String, String)]
showOpacity b = if b then [("opacity", "0.6")] else [("opacity", "1")]

showNotification :: Game -> String
showNotification (Game p b)
  | isOver b = (show $ findWinner b) ++ " player wins!"
  | otherwise = show p ++ "'s turn"

-- This is the main routine of the threepenny-gui package
-- Here, we hook up all of the events and behaviours to the
-- user interface
setup :: Window -> UI ()
setup window = void $ do
  return window # set title "Othello"

  -- Converts a url to an HTML image element
  let uiImg :: FilePath -> UI Element
      uiImg fp = UI.img # set UI.src fp
                        # set UI.style [("width","50px"),("height","50px")]

  imgs <- mapM uiImg initImgs
  
  -- Turn imgs into elements, and create click events for each image
  let uiCells :: [UI Element]
      uiCells = map element imgs

      clicks :: [Event ()]
      clicks =  map UI.click imgs

      -- Create an event that represents a Game Move
      -- associated with a click on a particular square
      ePlayer :: Event Move
      ePlayer = fmap concatenate . unions $ zipWith (\e s -> move s <$ e)
                clicks squares
  
  -- Cteates an event that represents the computer
  -- playing whenever the user clicks on the AI button
  ai <- UI.button # set UI.text "AI Move"
  let eComputer = (\s -> chooseMove s s) <$ (UI.click ai)
      moves = unionWith const ePlayer eComputer

  -- The Game state at the time of a click.
  -- accumE accumulates the moves event into a Games event,
  -- it's similar to fold
  eState <- accumE newGame moves

  -- A behavior representing the state of the game at any time.
  bState <- stepper newGame eState

  -- Create an element to indicate whose turn it is
  notification <- UI.h2

  let bNotify :: Behavior String
      bNotify = showNotification <$> bState

  -- The sink function hooks up a user interface element to
  -- a behaviour
  sink UI.text bNotify $ element notification

  -- Indicate legal moves by making squares translucent when cursor
  -- is over them
  let hoverSquares :: [Event Square]
      hoverSquares = zipWith (\e s -> s <$ e) (UI.hover <$> imgs) squares

      -- This event is true when you are no longer hovering over a
      -- square
      leaves :: [Event Bool]
      leaves = (fmap . fmap) (const False) (UI.leave <$> imgs)

      bLegal :: Behavior (Square -> Bool)
      bLegal = (\g -> isLegal (board g) (piece g)) <$> bState

      eHovering :: [Event Bool]
      eHovering = (\e -> bLegal <@> e) <$> hoverSquares

      eHovers :: [Event Bool]
      eHovers = zipWith (unionWith const) eHovering leaves

  bHovers <- mapM (stepper False) eHovers

  let bHoverStyle :: [Behavior [(String, String)]]
      bHoverStyle = (fmap . fmap) showOpacity bHovers

  zipWithM_ (\b e -> sink UI.style b e) bHoverStyle uiCells
  
  -- Update Board
  let setSrcs :: [FilePath] -> [UI Element] -> UI ()
      setSrcs fs es = zipWithM_ (set UI.src) fs es
      
  onChanges bState $ \g -> do
    setSrcs (toUrls g) uiCells

  getBody window #+ [ column
                      [ UI.h1 #+ [string "Othello"]
                      , grid (chunksOf 8 uiCells) # set UI.style [("line-height", "0")]
                      , UI.div #+ [element notification] # set UI.class_ "notification"
                      , element ai # set UI.style [ ("font", "bold 24px Optima")
                                                  , ("background-color", "#DDDDDD")
                                                  , ("color", "darkred")
                                                  , ("margin", "0 auto") ]
                      ] # set UI.style [("background-color","#DDDDDD")
                                       ,("text-align","center")
                                       ,("font-family","Optima, Arial, Helvetica, sans-serif")
                                       ,("margin","0 auto")
                                       ,("border","solid 3px #CACACA")]
                    ]
