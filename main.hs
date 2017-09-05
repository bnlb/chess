import Board
import Moves
import Piece


main = do
   putStr "\n\nWelcome to Chess!\n\n"
   let board = setupBoard Black
   putStr $ getBoardString board
   putStr "\n\nPick your piece (e.g., a4, h1):\n"
   piece <- getLine
   print piece
