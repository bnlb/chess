module Moves (
  Move,
  Moves,
  getMoves,
  getSpacesByColor,
  getSpacesByRole,
  getMovesByColor,
  flattenMoves,
  hasMoves
) where

import Data.List (nub)
import Data.Maybe (isJust, fromJust)
import Board
import Piece
import Paths


-- This represents a space on the board a piece can move to.
type Move = SpaceId


-- This represents all the moves a piece can take across the board.
-- The first item in the tuple is the id of the space the piece is currently on,
-- and the second item is an array of all the space ids on the board that it
-- can move to.
type Moves = (SpaceId, [ Move ])


getKingMoves :: Space -> Board -> [ Move ]
getKingMoves space board =
  let id = getId space
      paths = getAllDiagonalPaths id ++ getAllStraightPaths id
      moves = concatMap (take 1) paths
      movesOnBoard = filter isOnBoard moves
  in filter (canMoveToSpace board space) movesOnBoard


getQueenMoves :: Space -> Board -> [ Move ]
getQueenMoves space board =
  let id = getId space
      paths = (++) <$> getAllDiagonalPaths <*> getAllStraightPaths $ id
  in getMovesFromPaths space board paths


getRookMoves :: Space -> Board -> [ Move ]
getRookMoves space board =
  let paths = getAllStraightPaths $ getId space
  in getMovesFromPaths space board paths


getBishopMoves :: Space -> Board -> [ Move ] 
getBishopMoves space board =
  let paths = getAllDiagonalPaths $ getId space
  in getMovesFromPaths space board paths


getKnightMoves :: Space -> Board -> [ Move ]
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
  in filter (canMoveToSpace board space) movesOnBoard


getPawnMoves :: Space -> Board -> [ Move ]
getPawnMoves space board = 
  let piece = getContent space
  in case piece of 
    (Just Piece { hasMoved = False }) -> getFirstPawnMoves space board
    (Just Piece { hasMoved = True }) -> getAdditionalPawnMoves space board


getFirstPawnMoves :: Space -> Board -> [ Move ]
getFirstPawnMoves space board =
  let piece = getContent space
      id = getId space
      getPath = case piece of
        Just (Piece { getDirection = Up }) -> getPathN
        Just (Piece { getDirection = Down }) -> getPathS
        _ -> (\_ -> [])
      path = getPath id
      moves = take 2 path
  in filter (canMoveToSpace board space) moves


getAdditionalPawnMoves :: Space -> Board -> [ Move ]
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
  in filter (canMoveToSpace board space) moves


-- True if the given space id is either empty or contains an enemy piece.
canMoveToSpace :: Board -> Space -> SpaceId -> Bool
canMoveToSpace board space = (not . areFriendly space . getSpaceById board)


-- Converts an array of paths a piece can travel along into space ids
-- a piece can travel to during its turn.
getMovesFromPaths :: Space -> Board -> [ Path ] -> [ Move ]
getMovesFromPaths space board paths =
  let spaces = map (map (getSpaceById board)) paths
      moves = map (getUnobstructedMoves space) spaces
  in map getId $ concat moves


-- Returns a path if the first space in the path contains an enemy piece.
getPathIfEnemy :: (SpaceId -> Path) -> Space -> Board -> SpaceId -> Path
getPathIfEnemy getPath space board id =
  let hasPath = not . null $ getPath id
      pathStartId = head (getPath id)
      hasEnemy = hasPath && (areEnemies space $ getSpaceById board pathStartId)
  in [ id | hasEnemy ]


-- Given the id of a space and a board, return all of the space ids the piece
-- on the given id can move to.
getMoves :: SpaceId -> Board -> Moves
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
  in (id, getRoleMoves space board)


-- True if the piece on the given space id has spaces it can move to.
hasMoves :: Board -> SpaceId -> Bool
hasMoves board id = not . null . snd $ getMoves id board


-- Returns all spaces a player could move to for all of their pieces.
getMovesByColor :: Color -> Board -> [ Moves ]
getMovesByColor color board =
  let ids = map getId $ getSpacesByColor color board
      getValidMoves id = getMoves id board
  in map getValidMoves $ ids


 -- Trim the given moves to only contain those moves which occur before another 
 -- piece obstructs the rest of the path.
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


areEnemies:: Space -> Space -> Bool
areEnemies space1 space2 = 
  bothSpacesContainPieces space1 space2 &&
  getSpaceColor space1 /= getSpaceColor space2


areFriendly :: Space -> Space -> Bool
areFriendly space1 space2 = 
  bothSpacesContainPieces space1 space2 &&
  getSpaceColor space1 == getSpaceColor space2
  
  
getSpacesByColor :: Color -> Board -> [ Space ]
getSpacesByColor desiredColor = 
  filterBoardBySpaceContent (\piece ->
    isJust piece && (getPieceAttribute getColor piece == Just desiredColor))


getSpacesByRole :: Role -> Board -> [ Space ]
getSpacesByRole desiredRole =
  filterBoardBySpaceContent (\piece ->
    isJust piece && (getPieceAttribute getRole piece == Just desiredRole))


getSpaceColor :: Space -> Maybe Color
getSpaceColor space = getPieceAttribute getColor $ getContent space


flattenMoves :: [ Moves ] -> [ Move ]
flattenMoves = concatMap (\(id, moves) -> moves)