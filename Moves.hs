module Moves (
  getValidMoves
) where

import Data.Char (ord, chr)
import Data.List (nub)
import Data.Maybe (isJust, fromMaybe, fromJust)
import Board
import Piece
import Paths
import Func (funcAnd)


getKnightMoves :: Space -> Board -> [ SpaceId ]
getKnightMoves space board =
  let [ col, row ] = getId space
      moves = [
        [ increment 1 col, increment 2 row ],
        [ increment 1 col, decrement 2 row ],
        [ decrement 1 col, increment 2 row ],
        [ decrement 1 col, decrement 2 row ],
        [ increment 2 col, increment 1 row ],
        [ increment 2 col, decrement 1 row ],
        [ decrement 2 col, increment 1 row ],
        [ decrement 2 col, decrement 1 row ] ]
      movesOnBoard = filter isOnBoard moves
  in filter (not . areFriendly space . getSpaceById board) movesOnBoard


getQueenMoves :: Space -> Board -> [ SpaceId ]
getQueenMoves space board =
  let
    id = getId space
    paths = getAllDiagonalPaths id ++ getAllStraightPaths id
    spaces = map (map (getSpaceById board)) paths
    moves = map (getUnobstructedMoves space) spaces 
  in map getId $ concat moves


getRookMoves :: Space -> Board -> [ SpaceId ]
getRookMoves space board =
  let
    id = getId space
    paths = getAllStraightPaths id
    spaces = map (map (getSpaceById board)) paths
    moves = map (getUnobstructedMoves space) spaces 
  in map getId $ concat moves


getBishopMoves :: Space -> Board -> [ SpaceId ]
getBishopMoves space board =
  let
    id = getId space
    paths = getAllDiagonalPaths id
    spaces = map (map (getSpaceById board)) paths
    moves = map (getUnobstructedMoves space) spaces 
  in map getId $ concat moves


getKingMoves :: Space -> Board -> [ SpaceId ]
getKingMoves space board =
  let id = getId space
      paths = getAllDiagonalPaths id ++ getAllStraightPaths id
      moves = concatMap (take 1) paths
      movesOnBoard = filter isOnBoard moves
  in filter (not . areFriendly space . getSpaceById board) movesOnBoard


getPawnMoves :: Space -> Board -> [ SpaceId ]
getPawnMoves space board = 
  let piece = getContent space
      id = getId space
  in case piece of 
    (Just Piece { getDirection = Up, hasMoved = False }) ->
      let path = getPathN id
          moves = take 2 path
      in filter (not . areFriendly space . getSpaceById board) moves
    (Just Piece { getDirection = Down, hasMoved = False }) ->
      let path = getPathS id
          moves = take 2 path
      in filter (not . areFriendly space . getSpaceById board) moves
    (Just Piece { getDirection = Up, hasMoved = True }) ->
      let idNE = head (getPathNE id)
          idNW = head (getPathNW id)
          hasEnemyNE = areEnemies space $ getSpaceById board idNE
          hasEnemyNW = areEnemies space $ getSpaceById board idNW
          pathNE = [ idNE | hasEnemyNE ]
          pathNW = [ idNW | hasEnemyNW ]
          paths = [ getPathN id ] ++ [ pathNW ] ++ [ pathNE ]
          moves = concatMap (take 1) paths
      in filter (not . areFriendly space . getSpaceById board) moves
    (Just Piece { getDirection = Down, hasMoved = True }) ->
      let idSE = head (getPathSE id)
          idSW = head (getPathSW id)
          hasEnemySE = areEnemies space $ getSpaceById board idSE
          hasEnemySW = areEnemies space $ getSpaceById board idSW
          pathSE = [ idSE | hasEnemySE ]
          pathSW = [ idSW | hasEnemySW ]
          paths = [ getPathS id ] ++ [ pathSW ] ++ [ pathSE ]
          moves = concatMap (take 1) paths
      in filter (not . areFriendly space . getSpaceById board) moves


-- == To cleanup:

-- Get all the valid moves for the given space and board.
getValidMoves :: Space -> Board -> [ Space ]
getValidMoves space board = board


-- Returns all unique spaces a player could move to for all of their pieces.
getAllMovesByColor :: Color -> Board -> [ Space ]
getAllMovesByColor color board =
  let spaces = getAllOfColor color board
      getMoves space = getValidMoves space board
  in nub . concatMap getMoves $ spaces


-- True if a space contains a piece of the same color.
hasOwnPiece :: Maybe Color -> Space -> Bool
hasOwnPiece color space =
  (not $ isEmptySpace space) &&
  (getPieceAttribute getColor $ getContent space) == color 


getSpaceColor :: Space -> Maybe Color
getSpaceColor space = getPieceAttribute getColor $ getContent space


-- Trim the given moves to only contain those moves which occur
-- before another piece is in the way.
getUnobstructedMoves :: Space -> [ Space ] -> [ Space ]
getUnobstructedMoves space1 spaces = 
  let spacesWithIndexes = zip spaces [0..]
  in map fst $ takeWhile (\(spaceN, i) ->
    let previousItem = if i == 0 then Nothing else Just (spaces !! (i - 1))
    in if 
      areFriendly space1 spaceN ||
      (isJust previousItem && areEnemies space1 (fromJust previousItem))
      then False
      else True) spacesWithIndexes


bothSpacesContainPieces :: Space -> Space -> Bool
bothSpacesContainPieces space1 space2 = 
  (not $ isEmptySpace space1) && (not $ isEmptySpace space2)


areEnemies:: Space -> Space -> Bool
areEnemies space1 space2 = 
  bothSpacesContainPieces space1 space2 &&
  getSpaceColor space1 /= getSpaceColor space2


areFriendly :: Space -> Space -> Bool
areFriendly space1 space2 = 
  bothSpacesContainPieces space1 space2 &&
  getSpaceColor space1 == getSpaceColor space2
  
  
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
getPieceAttribute getAttr piece = piece >>= Just . getAttr

