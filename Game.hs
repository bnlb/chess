module Game (
  play
) where

import Data.List
import Data.Char (toLower)
import Board
import Moves
import Piece


play :: IO ()
play = do
  putStr "\n\nWelcome to Chess!\n\n"
  putStr "\nPick a color to play as: ('white', 'black') \n\n"
  player1Color <- fmap getColorFromInput getLine
  let board = setupBoard player1Color
  takeTurn player1Color board


takeTurn :: Color -> Board -> IO ()
takeTurn color board =
  if isInCheckmate color board
  then putStrLn "Checkmate!"
  else do
    putStr $ getBoardString board
    target <- getTarget color board
    let moves = snd $ getMoves target board
    destination <- getDestination target moves
    let updatedBoard = movePiece target destination board
    takeTurn (getOppositeColor color) updatedBoard


getTarget :: Color -> Board -> IO SpaceId
getTarget color board = do
  putStrLn "Pick your piece: (a4, h1, etc)"
  id <- fmap formatInput getLine
  let targets = map getId $ getAllOfColor color board
      targetsWithMoves = filter (hasMoves board) targets
  if id `elem` targetsWithMoves
    then
      return id
    else do 
      putStrLn "Invalid piece. Try again."
      getTarget color board


getDestination :: SpaceId -> [ SpaceId ] -> IO SpaceId
getDestination targetId moves = do
  putStrLn "Pick your destination: (a4, h1, etc)"
  id <- fmap formatInput getLine
  if id `elem` moves
    then
      return id
    else do
      putStrLn "Invalid move. Try again."
      getDestination targetId moves


-- Consider it checkmate if the king has at least one move left,
-- and all of the kings moves are contained in all of the opponent's
-- possible moves.

-- Consider it checkmate if both:
-- a) All of the kings moves are in all of the opponent's moves.
-- b) For all of the possible moves a player has, none of them
--    block the kings location from being in the opponent's moves.

-- Moving king
  -- all of moves in all of opps moves

-- Moving piece
  -- For all own possible moves, calc opps possible moves
  -- if any of those moves contain kings location


-- True if moving a piece from the target to the destination
-- removes the king's location from the enemies possible moves.
canSaveKing :: SpaceId -> Color -> Board -> (SpaceId, Move) -> Bool
canSaveKing kingId enemyColor board (pieceId, destinationId) =
  let updatedBoard = movePiece pieceId destinationId board
      enemyMoves = flattenMoves $ getAllMovesByColor enemyColor updatedBoard
  in notElem kingId enemyMoves


pieceCanSaveKing :: SpaceId -> [ Moves ] -> Color -> Board -> Bool
pieceCanSaveKing kingId kingTeamMoves enemyColor board =
  let flatten = concatMap (\(id, moves) -> map (\m -> (id, m)) moves)
  in all (canSaveKing kingId enemyColor board) (flatten kingTeamMoves)


isInCheckmate :: Color -> Board -> Bool
isInCheckmate color board =
  let opponentsColor = getOppositeColor color
      kingId = getId . head . getAllOfColor color . getAllOfRole King $ board
      kingsMoves = snd $ getMoves kingId board
      kingsTeam = getAllMovesByColor color board
      opponentsMoves = getAllMovesByColor opponentsColor board
      kingCanBeSaved = pieceCanSaveKing kingId opponentsMoves opponentsColor board
  in kingIsStuck kingsMoves opponentsMoves && not kingCanBeSaved


-- True if all of the king's moves are contained in all of opponent's moves.
kingIsStuck :: [ SpaceId ] -> [ Moves ] -> Bool
kingIsStuck kingsMoves opponentsMoves =
  null (kingsMoves \\ flattenMoves opponentsMoves) && (not . null) kingsMoves


movePiece :: SpaceId -> SpaceId -> Board -> Board
movePiece target destination board =
  let targetPiece = getPieceBySpaceId target board
      updatedPiece = fmap (\p -> p { hasMoved = True }) targetPiece
  in setSpace Nothing target $ setSpace updatedPiece destination board


getColorFromInput :: String -> Color
getColorFromInput input
  | x == 'b' = Black
  | otherwise = White
  where (x:xs) = map toLower input


-- Allow either '1a' or 'a1' as input.
formatInput :: String -> String
formatInput input
  | head lowercaseId `elem` [ '1'..'8' ] = reverse lowercaseId 
  | otherwise = lowercaseId
  where lowercaseId = map toLower input