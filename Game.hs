module Game (
  play
) where

import Data.List
import Data.Char (toLower)
import Board
import Moves
import Piece


-- Let's the user pick a color and kicks off the game.
play :: IO ()
play = do
  putStr "\n\nWelcome to Chess!\n\n"
  putStr "\nPick a color to play as: ('white', 'black') \n\n"
  player1Color <- fmap getColorFromInput getLine
  let board = setupBoard player1Color
  takeTurn player1Color board


-- Continue taking turns until checkmate occurs.
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


-- Prompts user for a piece to move until a valid option is chosen.
getTarget :: Color -> Board -> IO SpaceId
getTarget color board = do
  putStrLn "Pick your piece: (a4, h1, etc)"
  id <- fmap formatInput getLine
  let targets = map getId $ getSpacesByColor color board
      targetsWithMoves = filter (hasMoves board) targets
  if id `elem` targetsWithMoves
    then
      return id
    else do 
      putStrLn "Invalid piece. Try again."
      getTarget color board


-- Prompts user for a destination for their chosen piece until a valid option
-- is chosen.
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


-- Consider it checkmate if:
--   a) the king has spaces it can move to, and all of those spaces are also
--      spaces the opposing team's pieces can move to.
--   b) the king's team can't move any of its pieces in such a way as to prevent
--      the king's location from being a space the opposing team can move to.
isInCheckmate :: Color -> Board -> Bool
isInCheckmate color board =
  let opponentsColor = getOppositeColor color
      kingId = getId . head . getSpacesByColor color . getSpacesByRole King $ board
      kingsMoves = snd $ getMoves kingId board
      kingsTeamMoves = getMovesByColor color board
      opponentsMoves = getMovesByColor opponentsColor board
      kingCanBeSaved = teamCanSaveKing kingId kingsTeamMoves opponentsColor board
  in kingIsStuck kingsMoves opponentsMoves && not kingCanBeSaved


-- True if moving a piece from the target to the destination
-- removes the king's location from the enemies possible moves.
canSaveKing :: SpaceId -> Color -> Board -> (SpaceId, Move) -> Bool
canSaveKing kingId enemyColor board (pieceId, destinationId) =
  let updatedBoard = movePiece pieceId destinationId board
      enemyMoves = flattenMoves $ getMovesByColor enemyColor updatedBoard
  in notElem kingId enemyMoves


-- True if any of the pieces on the same team as the king can move in such a
-- way as to prevent the king's location from being in one of the opposing teams
-- next moves.
teamCanSaveKing :: SpaceId -> [ Moves ] -> Color -> Board -> Bool
teamCanSaveKing kingId kingTeamMoves enemyColor board =
  let flatten = concatMap (\(id, moves) -> map (\m -> (id, m)) moves)
  in all (canSaveKing kingId enemyColor board) (flatten kingTeamMoves)


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