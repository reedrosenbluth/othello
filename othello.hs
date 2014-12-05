{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Main where

import Data.Array
import Data.List.Split
import Control.Monad
import System.Directory

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

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

  let bPieceUrls :: Behavior [FilePath]
      bPieceUrls = toUrls <$> bState

  -- The file path of the piece about to be played
  -- let bPieceUrl :: Behavior FilePath
  --     bPieceUrl = (getPieceUrl . piece) <$> bState

  let initImgs = replicate 27 (getPieceUrl Empty)
                 ++ [getPieceUrl White] ++ [getPieceUrl Black]
                 ++ replicate 6  (getPieceUrl Empty)
                 ++ [getPieceUrl Black] ++ [getPieceUrl White]
                 ++ replicate 27 (getPieceUrl Empty)
  
  -- A list of behaviours, one for each tile
  -- ps <- zipWithM (\e i -> stepper i (bPieceUrl <@ e)) events initImgs

  -- Connect each of these behaviours to the tiles on the GUI
  -- zipWithM_ (\b e -> sink UI.src b e) ps uiCells

  onEvent eState $ \g -> mapM_ (map (\i -> set UI.src i) (toUrls $ g)) uiCells

toUrls :: Game -> [FilePath]
toUrls (Game _ b) = [getPieceUrl $ b ! (x,y) | y <- [1..8], x <- [1..8]]

-- Game
data Direction = N | NE | E | SE
               | S | SW | W | NW
    deriving (Enum)

data Piece = Empty | Black | White
    deriving (Show, Eq)

type Move = Game -> Game

data Game = Game { piece :: Piece, board :: Board }

type Square = (Int, Int)
type Line   = [Square]

type Board = Array Square Piece

line :: Square -> Direction -> Line
line (x, y) N  = [(x, y + h)     | h <- [1..8], y+h <= 8]
line (x, y) S  = [(x, y - h)     | h <- [1..8], y-h >= 1]
line (x, y) E  = [(x + h, y)     | h <- [1..8], x+h <= 8]
line (x, y) W  = [(x - h, y)     | h <- [1..8], x-h >= 1]
line (x, y) NE = [(x + h, y + h) | h <- [1..8], y+h <= 8, x+h <= 8]
line (x, y) SE = [(x + h, y - h) | h <- [1..8], y-h >= 1, x+h <= 8]
line (x, y) NW = [(x - h, y + h) | h <- [1..8], y+h <= 8, x-h >= 1]
line (x, y) SW = [(x - h, y - h) | h <- [1..8], y-h >= 1, x-h >= 1]

pieces :: Board -> Line -> [Piece]
pieces brd = map (brd !)

opposite :: Piece -> Piece
opposite Black = White
opposite White = Black
opposite Empty = Empty

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

toFlip :: Board -> Piece -> Line -> Line
toFlip _ _ []   = []
toFlip _ _ (_:[]) = []
toFlip b p l
  | zs /= [] && fst (head zs) == p = map snd ys 
  | otherwise = []
  where
    a  = zip (pieces b l) l
    ys = takeWhile (\y -> fst y == opposite p) a
    zs = dropWhile (\y -> fst y == opposite p) a

toFlipAll :: Board -> Piece -> Square -> [Square]
toFlipAll b p s = concat [toFlip b p l | l <- map (line s) [N .. NW]]

flipBoard :: Board -> Piece -> Square -> Board
flipBoard b p s = b // ((s, p) : zip flips (repeat p))
  where
    flips = toFlipAll b p s

move :: Square -> Game -> Game
move square (Game p b) = Game piece' board'
  where
    board'  = flipBoard b p square
    piece' = case p of {Black -> White; White -> Black; Empty -> Empty}
