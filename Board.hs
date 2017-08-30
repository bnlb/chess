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
  isEmptySpace,
  getEmptySpaces,
  getOccupiedSpaces,
  filterBoardBySpaceContent,
  setSpaceContents
) where

import Data.Char (ord)
import Data.Maybe (isJust)
import Piece
import Array (slice)


-- A normal chess id for a space, e.g.: 'a1'
type SpaceId = String


-- Each space on the board contains a piece or is empty.
data Space = Space {
  getId :: SpaceId,
  getContent :: Maybe Piece
} deriving (Eq)


-- Our board is just an array of spaces. It's kept flat to make mapping/filtering
-- easier. Any logic related to how pieces can move across the board is done using
-- the ids (e.g., 'a3') of each piece, instead of using array indexes.
type Board = [ Space ]


-- Create and set a board using the given color. Top here refers to the side
-- of the board that appears on top when printed to the screen.
setupBoard :: Color -> Board
setupBoard topColor = setBoard topColor createBoard


-- Create a board without pieces.
createBoard :: Board
createBoard = 
  [ createSpace id | row <- [ 'a'..'h' ], col <- [ '1'..'8' ], let id = [ row, col ] ]
  where createSpace id = Space { getId = id, getContent = Nothing }


-- Given a board and a color to use on top, set both sides of the board.
setBoard :: Color -> Board -> Board
setBoard topColor = setBoardSide (getOppositeColor topColor) . reverse . setBoardSide topColor


-- Initialize pieces on one side of the board to the given color.
setBoardSide :: Color -> Board -> Board
setBoardSide color board = let
  roles = [ Rook, Bishop, Knight, King, Queen, Knight, Bishop, Rook ] ++ repeat Pawn
  boardToSet = slice 0 15 board
  restOfBoard = slice 16 63 board
  setToRole (space, role) = setSpaceContents (Just Piece { getColor = color, getRole = role, getDirection = All, hasMoved = False }) space
  in (zipWith (curry setToRole) boardToSet roles) ++ restOfBoard


-- Update the contents of a given space to contain a piece.
setSpaceContents :: Maybe Piece -> Space -> Space
setSpaceContents contents space = space { getContent = contents }


-- Returns a space on the board given an id.
getSpaceById :: SpaceId -> Board -> Space
getSpaceById targetId = head . filter (\space -> getId space == targetId)


-- Returns the contents of a space given an id.
getSpaceContentsById :: SpaceId -> Board -> Maybe Piece
getSpaceContentsById spaceId board = getContent $ getSpaceById spaceId board


filterBoardBySpaceContent :: (Maybe Piece -> Bool) -> Board -> [ Space ]
filterBoardBySpaceContent func = filter $ func . getContent


isEmptySpace :: Space -> Bool
isEmptySpace = isJust . getContent


getEmptySpaces :: Board -> [ Space ]
getEmptySpaces = filter isEmptySpace


getOccupiedSpaces :: Board -> [ Space ]
getOccupiedSpaces = filter (not . isEmptySpace)


isInSameColumn :: SpaceId -> SpaceId -> Bool
isInSameColumn (_:colA) (_:colB) = colA == colB


isInSameRow :: SpaceId -> SpaceId -> Bool
isInSameRow (rowA:_) (rowB:_) = rowA == rowB


isInRowAbove :: SpaceId -> SpaceId -> Bool
isInRowAbove (rowA:_) (rowB: _) = ord rowA < ord rowB


isInRowBelow :: SpaceId -> SpaceId -> Bool
isInRowBelow (rowA:_) (rowB: _) = ord rowA > ord rowB


-- Get the absolute value between two rows or two columns.
getAbs :: Char -> Char -> Int
getAbs a b = (abs $ ord a - ord b)


-- Allow our board to be printed.
-- instance Show Board where
--  show = map print