module Moves (
  getAvailableMoves
) where


import Data.List (nub)
import Data.Maybe (isJust, fromMaybe)
import Board
import Piece
import Func (funcAnd)


-- True if the given spaces are within a certain limit of each other.
isWithinXMoves :: Int -> SpaceId -> SpaceId -> Bool
isWithinXMoves limit (rowA:colA:[]) (rowB:colB:[]) =
  getAbs rowA rowB <= limit &&
  getAbs colA colB <= limit


-- e.g., rook moves
isStraightMove :: SpaceId -> SpaceId -> Bool
isStraightMove (rowA:colA:[]) (rowB:colB:[]) = rowA == rowB || colA == colB


-- e.g., bishop moves
isDiagonalMove :: SpaceId -> SpaceId -> Bool
isDiagonalMove (rowA:colA:[]) (rowB:colB:[]) =
  getAbs rowA rowB == getAbs colA colB


-- True if it's ahead either straight or diagonally.
isForward :: SpaceId -> SpaceId -> Bool
isForward id1 id2 = ((||) <$> isInSameColumn id1 <*> isDiagonalMove id1) id2


isKnightMove :: SpaceId -> SpaceId -> Bool
isKnightMove (rowA:colA:[]) (rowB:colB:[]) = 
  (getAbs rowA rowB == 1) && (getAbs colA colB == 2) ||
  (getAbs rowA rowB == 2) && (getAbs colA colB == 1)


isPawnMove :: Maybe Piece -> SpaceId -> SpaceId -> Bool
isPawnMove (Just Piece { getDirection = Up, hasMoved = False }) =
    funcAnd [ isWithinXMoves 2, isInRowAbove, isInSameColumn ]
isPawnMove (Just Piece { getDirection = Up, hasMoved = True }) =
    funcAnd [ isWithinXMoves 1, isInRowAbove, isForward ]
isPawnMove (Just Piece { getDirection = Down, hasMoved = False }) =
    funcAnd [ isWithinXMoves 2, isInRowBelow, isInSameColumn ]
isPawnMove (Just Piece { getDirection = Down, hasMoved = True }) =
    funcAnd [ isWithinXMoves 1, isInRowBelow, isForward ]


-- Given a space, returns a function that determins if a given space id
-- is a valid move for the piece on the given space.
getMoveValidator :: Space -> SpaceId -> Bool
getMoveValidator space id
  | role == Just King = isWithinXMoves 1 spaceId id
  | role == Just Queen = ((||) <$> isStraightMove id <*> isDiagonalMove id) spaceId
  | role == Just Bishop = isDiagonalMove spaceId id
  | role == Just Knight = isKnightMove spaceId id
  | role == Just Rook = isStraightMove spaceId id
  | role == Just Pawn = isPawnMove (getContent space) spaceId id
  | otherwise = False
  where spaceId = getId space
        role = getPieceAttribute getRole $ getContent space


-- Get all the valid moves for the given space and board.
-- Returns results as if no other pieces are on board.
getValidMoves :: Space -> Board -> [ Space ]
getValidMoves space = filter (getMoveValidator space . getId)


-- Gets all the spaces a piece can actually move to, e.g.,
-- spaces that follow the rules for that piece, and are empty
-- or contain an enemy piece.
getAvailableMoves :: Space -> Board -> [ Space ]
getAvailableMoves space = filter (canMoveToSpace color) . getValidMoves space
  where color = getPieceAttribute getColor $ getContent space


-- Returns all unique spaces a player could move to for all of their pieces.
getAllMovesByColor :: Color -> Board -> [ Space ]
getAllMovesByColor color board =
  let spaces = getAllOfColor color board
      getMoves space = getAvailableMoves space board
  in nub . concatMap getMoves $ spaces


-- True if space isn't occupied by piece of the same color.
canMoveToSpace :: Maybe Color -> Space -> Bool
canMoveToSpace color space = isEmptySpace space || hasEnemyPiece color space 
  
  
-- True if a space contains a piece of the opposite color.
hasEnemyPiece :: Maybe Color -> Space -> Bool
hasEnemyPiece color space =
  (not $ isEmptySpace space) &&
  (getPieceAttribute getColor $ getContent space) /= fmap getOppositeColor color 


-- All of the spaces on the board that contain pieces of a given color.
getAllOfColor :: Color -> Board -> [ Space ]
getAllOfColor desiredColor = 
  filterBoardBySpaceContent (\piece ->
    isJust piece && (getPieceAttribute getColor piece == Just desiredColor))


-- All of the spaces on the board that contain pieces of a given role.
getAllOfRole :: Role -> Board -> [ Space ]
getAllOfRole desiredRole =
  filterBoardBySpaceContent (\piece ->
    isJust piece && (getPieceAttribute getRole piece == Just desiredRole))


-- Helper that returns a given attribute for pieces.
getPieceAttribute :: (Piece -> a) -> Maybe Piece -> Maybe a
getPieceAttribute getAttr piece = piece >>= (\p -> Just (getAttr p))
  