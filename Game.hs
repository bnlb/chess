module Game (
  play
) where


import Data.List
import Data.Char (toLower)
import Board
import Moves
import Piece
import Debug.Trace (trace)


-- Consider it checkmate if all of king's possible moves
-- are within all of opponents possible moves.
isInCheckmate :: Color -> Board -> Bool
isInCheckmate color board =
  let opponentsColor = getOppositeColor color
      kingId = getId . head . getAllOfColor color . getAllOfRole King $ board
      kingsMoves = getMoves kingId board
      opponentsMoves = getAllMovesByColor (getOppositeColor color) board
  in null (kingsMoves \\ opponentsMoves)


play :: IO ()
play = do
  putStr "\n\nWelcome to Chess!\n\n"
  putStr "\nPick a color to play as: ('white', 'black') \n\n"
  player1Color <- fmap getColorFromInput getLine
  let board = setupBoard player1Color
  putStr $ getBoardString board
  takeTurn player1Color board


getColorFromInput :: String -> Color
getColorFromInput input
  | x == 'b' = Black
  | otherwise = White
  where (x:xs) = map toLower input


takeTurn :: Color -> Board -> IO ()
takeTurn color board =
  if isInCheckmate color board
  then putStr "Checkmate!"
  else do
    putStr $ getBoardString board
    target <- getTarget color board
    destination <- getDestination target board
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
  putStr "\n\nPick your piece: (a4, h1, etc)"
  id <- fmap formatInput getLine
  let possibleTargets = map getId $ getAllOfColor color board
  if id `elem` possibleTargets
    then 
      return id
    else do 
      putStr "\nInvalid piece. Try again."
      getTarget color board


getDestination :: SpaceId -> Board -> IO SpaceId
getDestination targetId board = do
putStr "\n\nPick your destination: (a4, h1, etc)"
id <- fmap formatInput getLine
let moves = getMoves id board
if id `elem` moves
  then 
    return id
  else do
    putStr "\nInvalid move. Try again."
    getDestination targetId board