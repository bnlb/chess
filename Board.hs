{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Board (
  SpaceId,
  Space(..),
  Board(..),
  setupBoard,
  isOnBoard,
  isEmptySpace,
  getSpaceById,
  getBoardColumns,
  getBoardRows,
  filterBoardBySpaceContent,
  getPieceBySpaceId,
  setSpace,
  getBoardString,
  increment,
  decrement
) where

import Data.Char (ord, chr)
import Data.Maybe (isNothing)
import Data.List (intersperse)
import Piece
import Array (slice)


-- An id for a space, e.g.: 'a1'
type SpaceId = String


-- Each space on the board contains a piece or is empty.
data Space = Space {
  getId :: SpaceId,
  getContent :: Maybe Piece
}


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
    row <- getBoardRows
    col <- getBoardColumns
    return $ Space { getId = [ col, row ], getContent = Nothing }


-- Given a board and a color to use on top, set both sides of the board.
setBoard :: Color -> Board -> Board
setBoard topColor =
  let opponentColor = getOppositeColor topColor
  in setBoardSide opponentColor Down . reverse . setBoardSide topColor Up


-- Initialize pieces on one side of the board to the given color.
setBoardSide :: Color -> Direction -> Board -> Board
setBoardSide color direction board = let
  roles = [ Rook, Knight, Bishop, King, Queen, Bishop, Knight, Rook ] ++ repeat Pawn
  boardToSet = slice 0 15 board
  restOfBoard = slice 16 63 board
  setToRole (space, role) = 
    let piece = getPiece role color (getDirectionForSpace direction role)
    in setSpaceContents (Just piece) space
  in (zipWith (curry setToRole) boardToSet roles) ++ restOfBoard


getDirectionForSpace :: Direction -> Role -> Direction
getDirectionForSpace direction Pawn = direction
getDirectionForSpace direction _ = All


-- True if a given space id is on the board.
isOnBoard :: SpaceId -> Bool
isOnBoard [ col, row ] = row `elem` getBoardRows && col `elem` getBoardColumns
isOnBoard _ = False


-- Return a new piece with some defaults applied.
getPiece:: Role -> Color -> Direction -> Piece
getPiece role color direction = 
  Piece { getColor = color, getRole = role, getDirection = direction, hasMoved = False }


-- Update a space to contain the given contents.
setSpaceContents :: Maybe Piece -> Space -> Space
setSpaceContents contents space = space { getContent = contents }


-- Update the board so that the given contents are on the given id.
setSpace :: Maybe Piece -> SpaceId -> Board -> Board
setSpace contents id board =
  map (
    \space -> 
    if getId space == id then setSpaceContents contents space else space
    ) board


getSpaceById :: Board -> SpaceId -> Space
getSpaceById board id = head $ filter (\space -> getId space == id) board


getPieceBySpaceId :: SpaceId -> Board -> Maybe Piece
getPieceBySpaceId spaceId board = getContent $ getSpaceById board spaceId


filterBoardBySpaceContent :: (Maybe Piece -> Bool) -> Board -> [ Space ]
filterBoardBySpaceContent func = filter (func . getContent)


isEmptySpace :: Space -> Bool
isEmptySpace = isNothing . getContent


-- Used to print the board.
getBoardString :: Board -> String
getBoardString board =
  let
    rows = getRows board []
    withIndex = zip rows [ 8, 7..1 ]
    rowsWithNums = map getRowWithNumbers withIndex
    finalRows = [ getLetterRow ] ++ rowsWithNums  ++  [ getLetterRow ]
  in unlines finalRows


-- Return a string used to mark columns on the board.
getLetterRow :: String
getLetterRow = 
  let row = concat . intersperse " " $ map (\n -> " " ++ [ n ] ++ " " ) [ 'a'..'h' ]
  in "  " ++ row ++ "  "


-- Return a string representation of a row.
getRow :: [ Space ] -> String
getRow row = reverse . concat . intersperse "|" $ map (\space -> show space) row


-- Return a string representation of a row with numbers added.
getRowWithNumbers :: ([ Space ], Int) -> String
getRowWithNumbers (row, i) = show i ++ " " ++ getRow row ++ " " ++ show i


-- Break up our flat board array into rows of 8 items each.
getRows :: Board -> [[ Space ]] -> [[ Space ]]
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
