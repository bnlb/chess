{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Board (
  SpaceId,
  Space(..),
  Board(..),
  getRows,
  getColumns,
  isOnBoard,
  setupBoard,
  isEmptySpace,
  getSpaceById,
  getPieceBySpaceId,
  setSpace,
  filterBySpaceContents,
  toString,
  increment,
  decrement
) where

import Data.Char (ord, chr)
import Data.Maybe (isNothing)
import Data.List (intersperse)
import Piece
import Array (slice)


-- An id for a space. e.g., 'a1'
type SpaceId = String


-- Each space on the board contains a piece or is empty.
data Space = Space {
  getId :: SpaceId,
  getContent :: Maybe Piece
}


-- Our board is just an array of spaces. It's kept flat to make mapping and
-- filtering easier. Any logic related to how pieces can move across the board
-- is done using the ids of each piece, instead of using array indexes.
type Board = [ Space ]


-- Allow our spaces to be printed.
instance Show Space where
  show Space { getContent = Nothing } = " _ "
  show Space { getContent = Just piece } = " " ++ show piece ++ " "


getRows :: [ Char ]
getRows = [ '1'..'8' ]


getColumns :: [ Char ]
getColumns = [ 'a'..'h' ]


isOnBoard :: SpaceId -> Bool
isOnBoard [ col, row ] = row `elem` getRows && col `elem` getColumns
isOnBoard _ = False


-- Create and set a board using the given color. The color will be used as
-- the color of the pieces on the side of the board used by the player who
-- takes the first turn.
setupBoard :: Color -> Board
setupBoard color = setBoard color createBoard


isEmptySpace :: Space -> Bool
isEmptySpace = isNothing . getContent


getSpaceById :: Board -> SpaceId -> Space
getSpaceById board id = head $ filter (\space -> getId space == id) board


getPieceBySpaceId :: SpaceId -> Board -> Maybe Piece
getPieceBySpaceId spaceId board = getContent $ getSpaceById board spaceId


-- Update the board so that the given contents are on the given id.
setSpace :: Maybe Piece -> SpaceId -> Board -> Board
setSpace contents id board =
  map 
  (\space -> 
    if getId space == id then setSpaceContents contents space else space
  )
  board


filterBySpaceContents :: (Maybe Piece -> Bool) -> Board -> [ Space ]
filterBySpaceContents func = filter (func . getContent)


-- Used to print the board.
toString :: Board -> String
toString board =
  let rows = splitIntoRows board []
      withIndex = zip rows [ 8, 7..1 ]
      rowString = map getRowStringWithRowNumbers withIndex
      finalRows = [ getColumnRow ] ++ rowString  ++  [ getColumnRow ]
  in unlines finalRows


-- Used to increment rows or columns.
increment :: Int -> Char -> Char
increment val c = chr $ ord c + val


-- Used to decrement rows or columns.
decrement :: Int -> Char -> Char
decrement val c = chr $ ord c - val


-- Private


-- Create a board without pieces.
createBoard :: Board
createBoard =
  do
    row <- getRows
    col <- getColumns
    return $ Space { getId = [ col, row ], getContent = Nothing }


-- Set both sides of the board to opposite colors.
setBoard :: Color -> Board -> Board
setBoard color =
  let opponentColor = getOppositeColor color
  in setBoardSide opponentColor Down . reverse . setBoardSide color Up


-- Set pieces on one side of the board to the given color.
setBoardSide :: Color -> Direction -> Board -> Board
setBoardSide color direction board = 
  let roles = [ Rook, Knight, Bishop, King, Queen, Bishop, Knight, Rook ] ++ repeat Pawn
      boardToSet = slice 0 15 board
      restOfBoard = slice 16 63 board
      setToRole (space, role) = 
        let piece = getPiece role color (getDirectionForPiece direction role)
        in setSpaceContents (Just piece) space
  in (zipWith (curry setToRole) boardToSet roles) ++ restOfBoard


getDirectionForPiece :: Direction -> Role -> Direction
getDirectionForPiece direction Pawn = direction
getDirectionForPiece direction _ = All


getPiece:: Role -> Color -> Direction -> Piece
getPiece role color direction = 
  Piece { 
    getColor = color,
    getRole = role,
    getDirection = direction,
    hasMoved = False
  }


setSpaceContents :: Maybe Piece -> Space -> Space
setSpaceContents contents space = space { getContent = contents }


-- Return a string used to mark columns on the board.
getColumnRow :: String
getColumnRow = 
  let row = concat . intersperse " " $ map (\n -> " " ++ [ n ] ++ " " ) getColumns
  in "  " ++ row ++ "  "


getRowString :: [ Space ] -> String
getRowString row =
  reverse . concat . intersperse "|" $ map (\space -> show space) row


getRowStringWithRowNumbers :: ([ Space ], Int) -> String
getRowStringWithRowNumbers (row, i) =
  show i ++ " " ++ getRowString row ++ " " ++ show i


-- Break our flat board array into rows of 8 spaces each.
splitIntoRows :: Board -> [[ Space ]] -> [[ Space ]]
splitIntoRows [] built = built
splitIntoRows board built = 
  let nextSegment = take 8 board
      restOfBoard = drop 8 board
  in splitIntoRows restOfBoard $ built ++ [ nextSegment ]
