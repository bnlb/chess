import Data.List

-- Consider it checkmate if all of king's possible moves
-- are within all of opponents possible moves.
isInCheckmate :: Color -> Board -> Boolean
isInCheckmate color board =
  let opponentsColor = getOppositeColor color
      king = head . filter (\space -> getColor space == color) $ getAllOfRole King board 
      kingsMoves = getMoves king board
      opponentsMoves = getAllMovesByColor (getOppositeColor color) board
  in null (kingsValidMoves \\ opponentsMoves)


play :: Color -> Board -> SpaceId -> SpaceId -> Board