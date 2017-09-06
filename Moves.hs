module Moves (
  getMoves,
  getAllOfColor,
  getAllOfRole,
  getAllMovesByColor
) where

import Data.Char (ord, chr)
import Data.List (nub)
import Data.Maybe (isJust, fromMaybe, fromJust)
import Debug.Trace (trace)
import Board
import Piece
import Paths
import Func (funcAnd)


getKingMoves :: Space -> Board -> [ SpaceId ]
getKingMoves space board =
  let id = getId space
      paths = getAllDiagonalPaths id ++ getAllStraightPaths id
      moves = concatMap (take 1) paths
      movesOnBoard = filter isOnBoard moves
  in filter (not . areFriendly space . getSpaceById board) movesOnBoard


getQueenMoves :: Space -> Board -> [ SpaceId ]
getQueenMoves space board =
  let id = getId space
      paths = (++) <$> getAllDiagonalPaths <*> getAllStraightPaths $ id
  in getMovesFromPaths space board paths


getRookMoves :: Space -> Board -> [ SpaceId ]
getRookMoves space board =
  let paths = getAllStraightPaths $ getId space
  in getMovesFromPaths space board paths


getBishopMoves :: Space -> Board -> [ SpaceId ]
getBishopMoves space board =
  let paths = getAllDiagonalPaths $ getId space
  in getMovesFromPaths space board paths


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


getPawnMoves :: Space -> Board -> [ SpaceId ]
getPawnMoves space board = 
  let piece = getContent space
  in case piece of 
    (Just Piece { hasMoved = False }) -> getFirstPawnMoves space board
    (Just Piece { hasMoved = True }) -> getAdditionalPawnMoves space board


getFirstPawnMoves :: Space -> Board -> [ SpaceId ]
getFirstPawnMoves space board =
  let piece = getContent space
      id = getId space
      getPath = case piece of
        Just (Piece { getDirection = Up }) -> getPathN
        Just (Piece { getDirection = Down }) -> getPathS
        _ -> (\_ -> [])
      path = getPath id
      moves = take 2 path
  in filter (not . areFriendly space . getSpaceById board) moves


getAdditionalPawnMoves :: Space -> Board -> [ SpaceId ]
getAdditionalPawnMoves space board =
  let piece = getContent space
      id = getId space
      getPathLeft = case piece of
        (Just Piece { getDirection = Up }) -> getPathNW
        (Just Piece { getDirection = Down }) -> getPathSW
      getPathRight = case piece of
        (Just Piece { getDirection = Up }) -> getPathNE
        (Just Piece { getDirection = Down }) -> getPathSE
      getPathMain = case piece of
        (Just Piece { getDirection = Up }) -> getPathN
        (Just Piece { getDirection = Down }) -> getPathS
      paths = [ getPathMain id ] ++
              [ getPathIfEnemy getPathLeft space board id ] ++
              [ getPathIfEnemy getPathRight space board id ]
      moves = concatMap (take 1) paths
  in filter (not . areFriendly space . getSpaceById board) moves


-- Converts an array of paths a piece can travel along into space ids
-- a piece can travel to during its turn.
getMovesFromPaths :: Space -> Board -> [ Path ] -> [ SpaceId ]
getMovesFromPaths space board paths =
  let spaces = map (map (getSpaceById board)) paths
      moves = map (getUnobstructedMoves space) spaces
  in map getId $ concat moves


-- Returns a path if the first space in the path contains an enemy piece.
getPathIfEnemy :: (SpaceId -> Path) -> Space -> Board -> SpaceId -> Path
getPathIfEnemy getPath space board id =
  let pathStartId = head (getPath id)
      hasEnemy = areEnemies space $ getSpaceById board pathStartId
  in [ id | hasEnemy ]


-- Given a space and a board, return all the spaces a piece can move to.
getMoves :: SpaceId -> Board -> [ SpaceId ]
getMoves id board =
  let role = getPieceAttribute getRole . getContent $ getSpaceById board id
      space = getSpaceById board id
      getRoleMoves = case role of
        Just King -> getKingMoves
        Just Queen -> getQueenMoves
        Just Rook -> getRookMoves
        Just Bishop -> getBishopMoves
        Just Knight -> getKnightMoves
        Just Pawn -> getPawnMoves
        Nothing -> (\_ _ -> [])
  in getRoleMoves space board


-- Returns all unique spaces a player could move to for all of their pieces.
getAllMovesByColor :: Color -> Board -> [ SpaceId ]
getAllMovesByColor color board =
  let ids = map getId $ getAllOfColor color board
      getValidMoves id = getMoves id board
  in nub . concatMap getValidMoves $ ids


-- True if a space contains a piece of the same color.
hasOwnPiece :: Maybe Color -> Space -> Bool
hasOwnPiece color space =
  not (isEmptySpace space) &&
  getPieceAttribute getColor (getContent space) == color 


-- Trim the given moves to only contain those moves which occur
-- before another piece is in the way.
getUnobstructedMoves :: Space -> [ Space ] -> [ Space ]
getUnobstructedMoves space1 spaces = 
  let spacesWithIndexes = zip spaces [0..]
  in map fst $ takeWhile (
    \(spaceN, i) ->
      let previousItem = if i == 0 then Nothing else Just (spaces !! (i - 1))
      in not (
        areFriendly space1 spaceN ||
        (isJust previousItem && areEnemies space1 (fromJust previousItem))
      )
    ) spacesWithIndexes


bothSpacesContainPieces :: Space -> Space -> Bool
bothSpacesContainPieces space1 space2 = 
  not (isEmptySpace space1) && not (isEmptySpace space2)


-- True if two spaces are on opposite teams.
areEnemies:: Space -> Space -> Bool
areEnemies space1 space2 = 
  bothSpacesContainPieces space1 space2 &&
  getSpaceColor space1 /= getSpaceColor space2


-- True if two spaces are on the same team.
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


getSpaceColor :: Space -> Maybe Color
getSpaceColor space = getPieceAttribute getColor $ getContent space