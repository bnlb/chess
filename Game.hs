module Game (
  play
) where


import Data.List
import Data.Char (toLower)
import Board
import Moves
import Piece
import Debug.Trace (trace)


-- Consider it checkmate if the king has at least one move left,
-- and all of the kings moves are contained in all of the opponent's
-- possible moves.
isInCheckmate :: Color -> Board -> Bool
isInCheckmate color board =
  let opponentsColor = getOppositeColor color
      kingId = getId . head . getAllOfColor color . getAllOfRole King $ board
      kingsMoves = getMoves kingId board
      opponentsMoves = getAllMovesByColor (getOppositeColor color) board
  in null (kingsMoves \\ opponentsMoves) && (not . null) kingsMoves


play :: IO ()
play = do
  putStr "\n\nWelcome to Chess!\n\n"
  putStr "\nPick a color to play as: ('white', 'black') \n\n"
  player1Color <- fmap getColorFromInput getLine
  let board = setupBoard player1Color
  takeTurn player1Color board


getColorFromInput :: String -> Color
getColorFromInput input
  | x == 'b' = Black
  | otherwise = White
  where (x:xs) = map toLower input


takeTurn :: Color -> Board -> IO ()
takeTurn color board =
  if isInCheckmate color board
  then print "Checkmate!"
  else do
    putStr $ getBoardString board
    target <- getTarget color board
    let moves = getMoves target board
    destination <- getDestination target moves
    let updatedBoard = movePiece target destination board
    takeTurn (getOppositeColor color) updatedBoard


movePiece :: SpaceId -> SpaceId -> Board -> Board
movePiece target destination board =
  let targetPiece = getPieceBySpaceId target board
  in setSpace Nothing target $ setSpace targetPiece destination board


-- Allow users to enter '1a' as an id instead of 'a1'
formatInput :: String -> String
formatInput input
  | head lowercaseId `elem` [ '1'..'8' ] = reverse lowercaseId 
  | otherwise = lowercaseId
  where lowercaseId = map toLower input


getTarget :: Color -> Board -> IO SpaceId
getTarget color board = do
  print "Pick your piece: (a4, h1, etc)"
  id <- fmap formatInput getLine
  let targets = map getId $ getAllOfColor color board
      targetsWithMoves = filter (hasMoves board) targets
  if id `elem` targetsWithMoves
    then
      return id
    else do 
      print "Invalid piece. Try again."
      getTarget color board


getDestination :: SpaceId -> [ SpaceId ] -> IO SpaceId
getDestination targetId moves = do
  print "Pick your destination: (a4, h1, etc)"
  id <- fmap formatInput getLine
  if id `elem` moves
    then
      return id
    else do
      print "Invalid move. Try again."
      getDestination targetId moves