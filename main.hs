import Board
import Moves
import Piece


main = do
   print "Welcome to Chess!"
   let board = setupBoard Black
   putStr $ getBoardString board
