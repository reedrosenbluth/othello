module Main where

import Data.Array
import Data.List.Split
import Control.Monad
import Data.Maybe
import System.Directory
import System.FilePath

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Debug.Trace
traceShow' a = traceShow a a

main :: IO ()
main = do
  currentDirectory <- getCurrentDirectory
  let static = currentDirectory ++ "/static"
  startGUI defaultConfig { tpStatic = Just static } setup


setup :: Window -> UI ()
setup window = void $ do
  return window # set title "Othello"
  -- UI.addStyleSheet window "style.css"

  -- Create 64 empty tile images
  let emptyImg = UI.img # set UI.src "static/images/tile.png"
                        # set UI.style [("width","50px"),("height","50px")]

  imgs <- replicateM 64 emptyImg
  
  -- Turn our images into elements, and create events for each image
  let uiCells = map element imgs
      events = map UI.click imgs

      -- Create a stream of events
      moves :: Event Move
      moves = fmap concatenate . unions $ zipWith (\e s -> move s <$ e)
              events [(x,y) | y <- [1..8], x <- [1..8]]

  getBody window #+ [grid (chunksOf 8 uiCells) # set UI.style [("line-height", "0")]]

  -- The Game state at the time of a click
  eState <- accumE newGame moves

  -- A behavior; a function from time t to Game
  bState <- stepper newGame eState

  -- The file path of the piece about to be played
  let bPieceUrl :: Behavior FilePath
      bPieceUrl = (getPieceUrl . player) <$> bState

  let initImgs = replicate 27 (getPieceUrl Empty)
                 ++ [getPieceUrl White] ++ [getPieceUrl Black]
                 ++ replicate 6  (getPieceUrl Empty)
                 ++ [getPieceUrl Black] ++ [getPieceUrl White]
                 ++ replicate 27 (getPieceUrl Empty)
  
  -- A list of behaviours, one for each tile
  -- pieces <- mapM (\e -> stepper "static/images/tile.png" (bPieceUrl <@ e)) events
  pieces <- zipWithM (\e i -> stepper i (bPieceUrl <@ e)) events initImgs

  -- Connect each of these behaviours to the tiles on the GUI
  zipWithM_ (\b e -> sink UI.src b e) pieces uiCells

-- Game
data Direction = N | NE | E | SE
               | S | SW | W | NW

data Piece = Empty | Black | White
    deriving (Show, Eq)

type Move = Game -> Game

data Game = Game { player :: Piece, board :: Board }

type Square = (Int, Int)
type Line   = [Square]

type Board = Array Square Piece

line :: Board -> Direction -> Square -> Line
line board N  (x, y) = [(x, y + h)     | h <- [1..8], y+h <= 8]
line board S  (x, y) = [(x, y - h)     | h <- [1..8], y-h >= 1]
line board E  (x, y) = [(x + h, y)     | h <- [1..8], x+h <= 8]
line board W  (x, y) = [(x - h, y)     | h <- [1..8], x-h >= 1]
line board NE (x, y) = [(x + h, y + h) | h <- [1..8], y+h <= 8, x+h <= 8]
line board SE (x, y) = [(x + h, y - h) | h <- [1..8], y-h >= 1, x+h <= 8]
line board NW (x, y) = [(x - h, y + h) | h <- [1..8], y+h <= 8, x-h >= 1]
line board SW (x, y) = [(x - h, y - h) | h <- [1..8], y-h >= 1, x-h >= 1]

pieces :: Board -> Line -> [Piece]
pieces board = map (board !)

newBoard :: Board
newBoard = emptyArray // [((4,4), Black),((4,5), White),((5,4), White),((5,5), Black)]
  where
    emptyArray = listArray ((1,1),(8,8)) (repeat Empty)


newGame :: Game
newGame = Game Black newBoard

setSquare :: Board -> Square -> Piece -> Board
setSquare brd square piece =
    if (brd ! square) /= Empty
    then error $ "square " ++ show square ++ " is not empty"
    else brd // [(square, piece)]

getPieceUrl :: Piece -> FilePath
getPieceUrl Empty = "static/images/tile.png"
getPieceUrl Black = "static/images/black.png"
getPieceUrl White = "static/images/white.png"

move :: Square -> Game -> Game
move square (Game plyr brd) = Game player' board'
    where
    board'  = setSquare brd square plyr
    player' = case plyr of {Black -> White; White -> Black; Empty -> Empty}
