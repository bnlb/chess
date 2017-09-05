{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Board (
  SpaceId,
  Space(..),
  Board(..),
  setupBoard,
  getAbs,
  isInRowAbove,
  isInRowBelow,
  isInSameColumn,
  isInSameRow,
  isOnBoard,
  isEmptySpace,
  getEmptySpaces,
  getOccupiedSpaces,
  getSpaceById,
  filterBoardBySpaceContent,
  setSpaceContents,
  getBoardString,
  increment,
  decrement
) where

import Data.Char (ord, chr)
import Data.Maybe (isJust, isNothing)
import Data.List (intersperse)
import Piece
import Array (slice)


-- An id for a space, e.g.: 'a1'
type SpaceId = String


-- Each space on the board contains a piece or is empty.
data Space = Space {
  getId :: SpaceId,
  getContent :: Maybe Piece
} deriving (Eq)


-- Our board is just an array of spaces. It's kept flat to make mapping/filtering
-- easier. Any logic related to how pieces can move across the board is done
-- using the ids (e.g., 'a3') of each piece, instead of using array indexes.
type Board = [ Space ]


-- Allow our spaces to be printed.
instance Show Space where
  show Space { getContent = Nothing } = " _ "
  show Space { getContent = Just piece } = " " ++ show piece ++ " "


getBoardRows :: [ Char ]
getBoardRows = [ '1'..'8' ]


getBoardColumns :: [ Char ]
getBoardColumns = [ 'a'..'h' ]


-- Create and set a board using the given color. Top here refers to the side
-- of the board that appears on top when printed to the screen.
setupBoard :: Color -> Board
setupBoard topColor = setBoard topColor createBoard


-- Create a board without pieces.
createBoard :: Board
createBoard =
  do
    col <- getBoardColumns
    row <- getBoardRows
    return Space { getId = [ col, row ], getContent = Nothing }


-- Given a board and a color to use on top, set both sides of the board.
setBoard :: Color -> Board -> Board
setBoard topColor =
  setBoardSide (getOppositeColor topColor) . reverse . setBoardSide topColor


-- True if a given space id is on the board.
isOnBoard :: SpaceId -> Bool
isOnBoard [ col, row ] = row `elem` getBoardRows && col `elem` getBoardColumns


-- Initialize pieces on one side of the board to the given color.
setBoardSide :: Color -> Board -> Board
setBoardSide color board = let
  roles = [ Rook, Knight, Bishop, King, Queen, Bishop, Knight, Rook ] ++ repeat Pawn
  boardToSet = slice 0 15 board
  restOfBoard = slice 16 63 board
  setToRole (space, role) = setSpaceContents (Just (getPiece role color)) space
  in (zipWith (curry setToRole) boardToSet roles) ++ restOfBoard


-- Return a new piece with some defaults applied.
getPiece:: Role -> Color -> Piece
getPiece role color = 
  Piece { getColor = color, getRole = role, getDirection = All, hasMoved = False }


-- Update the contents of a given space to contain a piece.
setSpaceContents :: Maybe Piece -> Space -> Space
setSpaceContents contents space = space { getContent = contents }


-- Returns a space on the board given an id.
getSpaceById :: Board -> SpaceId -> Space
getSpaceById board id = head $ filter (\space -> getId space == id) board


-- Returns the contents of a space given an id.
getSpaceContentsById :: SpaceId -> Board -> Maybe Piece
getSpaceContentsById spaceId board = getContent $ getSpaceById board spaceId


filterBoardBySpaceContent :: (Maybe Piece -> Bool) -> Board -> [ Space ]
filterBoardBySpaceContent func = filter $ func . getContent


isEmptySpace :: Space -> Bool
isEmptySpace = isNothing . getContent


getEmptySpaces :: Board -> [ Space ]
getEmptySpaces = filter isEmptySpace


getOccupiedSpaces :: Board -> [ Space ]
getOccupiedSpaces = filter (not . isEmptySpace)


isInSameColumn :: SpaceId -> SpaceId -> Bool
isInSameColumn [ colA, _ ] [ colB, _ ] = colA == colB


isInSameRow :: SpaceId -> SpaceId -> Bool
isInSameRow [ _, rowA ] [ _, rowB ] = rowA == rowB


isInRowAbove :: SpaceId -> SpaceId -> Bool
isInRowAbove [ _, rowA ] [ _, rowB ] = ord rowA < ord rowB


isInRowBelow :: SpaceId -> SpaceId -> Bool
isInRowBelow [ _, rowA] [ _, rowB ] = ord rowA > ord rowB


-- Get the absolute value between two rows or two columns.
getAbs :: Char -> Char -> Int
getAbs a b = (abs $ ord a - ord b)


-- Used to print the board.
getBoardString :: Board -> String
getBoardString board =
  let
    rows = getRows board []
    withIndex = zip rows [8, 7..1]
    rowsWithNums = map getRowWithNumbers withIndex
    finalRows = [ getLetterRow ] ++ rowsWithNums  ++  [ getLetterRow ]
  in unlines finalRows


-- Return a string used to mark columns on the board.
getLetterRow :: String
getLetterRow = 
  let row = concat . intersperse " " $ map (\n -> " " ++ [ n ] ++ " " ) [ 'a'..'h' ]
  in "  " ++ row ++ "  "


-- Return a string representation of a row.
getRow :: [Space] -> String
getRow row = concat . intersperse "|" $ map (\space -> show space) row


-- Return a string representation of a row with numbers added.
getRowWithNumbers :: ([Space], Int) -> String
getRowWithNumbers (row, i) = show i ++ " " ++ getRow row ++ " " ++ show i


-- Break up our flat board array into rows of 8 items each.
getRows :: Board -> [[Space]] -> [[Space]]
getRows [] built = built
getRows board built = 
  let nextSegment = take 8 board
      restOfBoard = drop 8 board
  in getRows restOfBoard $ built ++ [ nextSegment ]


-- Used to increment rows or columns.
increment :: Int -> Char -> Char
increment val c = chr $ ord c + val


-- Used to decrement rows or columns.
decrement :: Int -> Char -> Char
decrement val c = chr $ ord c - val
