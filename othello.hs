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
  let uiImg :: FilePath -> UI Element
      uiImg fp = UI.img # set UI.src fp
                        # set UI.style [("width","50px"),("height","50px")]

  let initImgs = replicate 27 (getPieceUrl Empty)
                 ++ [getPieceUrl White] ++ [getPieceUrl Black]
                 ++ replicate 6  (getPieceUrl Empty)
                 ++ [getPieceUrl Black] ++ [getPieceUrl White]
                 ++ replicate 27 (getPieceUrl Empty)

  imgs <- mapM uiImg initImgs
  
  -- Turn our images into elements, and create events for each image
  let uiCells = map element  imgs
      clicks =  map UI.click imgs

      hovers :: [Event Bool]
      hovers = (fmap . fmap) (const True)  (UI.hover <$> imgs)
      leaves :: [Event Bool]
      leaves = (fmap . fmap) (const False) (UI.leave <$> imgs)

      -- Create a stream of events
      moves :: Event Move
      moves = fmap concatenate . unions $ zipWith (\e s -> move s <$ e)
              clicks [(x,y) | y <- [1..8], x <- [1..8]]

  -- The Game state at the time of a click
  eState <- accumE newGame moves

  -- A behavior; a function from time t to Game
  bState <- stepper newGame eState

  -- Set Notification
  notification <- UI.h2
  let bNotify :: Behavior String
      bNotify = showNotification <$> bState

  sink UI.text bNotify $ element notification

  -- Set hover state
  let eHovers = zipWith (unionWith (\a b -> a)) hovers leaves
  bHovers <- mapM (stepper False) eHovers
  let bHoverStyle = (fmap . fmap) showOpacity bHovers

  zipWithM_ (\b e -> sink UI.style b e) bHoverStyle uiCells
  
  -- Update Board
  let setSrcs :: [FilePath] -> [UI Element] -> UI ()
      setSrcs fs es = zipWithM_ (set UI.src) fs es
      
  onChanges bState $ \g -> do
    setSrcs (toUrls g) uiCells

  getBody window #+ [ column
                      [ UI.h1 #+ [string "Othello"]
                      , grid (chunksOf 8 uiCells) # set UI.style [("line-height", "0")]
                      , element notification
                      ]
                    ]


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
newBoard = emptyArray // [((4,4), White),((4,5), Black),((5,4), Black),((5,5), White)]
  where
    emptyArray = listArray ((1,1),(8,8)) (repeat Empty)

newGame :: Game
newGame = Game Black newBoard

setSquare :: Board -> Square -> Piece -> Board
setSquare brd square p =
    if (brd ! square) /= Empty
    then error $ "square " ++ show square ++ " is not empty"
    else brd // [(square, p)]

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

isLegal :: Board -> Piece -> Square -> Bool
isLegal b p s = b ! s == Empty && (not . null $ toFlipAll b p s)

move :: Square -> Game -> Game
move square g@(Game p b)
  | isLegal b p square = Game piece' board'
  | otherwise          = g
  where
    board'  = flipBoard b p square
    q = opposite p
    piece'
      | any (isLegal board' q ) [(x, y) | y <- [1..8], x <- [1..8]] = q
      | otherwise = p

isOver :: Board -> Bool
isOver b = not (any (isLegal b Black) squares || any (isLegal b White) squares)
  where
    squares = [(x, y) | y <- [1..8], x <- [1..8]] 

findWinner :: Board -> Piece
findWinner b
  | isOver b = case compare black white of
      GT -> Black
      LT -> White
      EQ -> Empty -- We use Empty to indicate a draw.
  | otherwise = error "The game is not over"
  where
    squares = [(x,y) | y <- [1..8], x <- [1..8]]
    black = length $ filter (\s -> b ! s == Black) squares
    white = length $ filter (\s -> b ! s == White) squares

showNotification :: Game -> String
showNotification (Game p b)
  | isOver b = (show $ findWinner b) ++ " player wins!"
  | otherwise = show p ++ "'s turn"

showOpacity :: Bool -> [(String, String)]
showOpacity b = if b then [("opacity", "0.8")] else [("opacity", "1")]
