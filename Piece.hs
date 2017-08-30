module Piece (
  Piece(..),
  Color(..),
  Role(..),
  Direction(..),
  getOppositeColor
) where


-- Some basic enums to describe pieces on the board.
data Color = White | Black deriving (Eq, Show)
data Role = King | Queen | Bishop | Knight | Rook | Pawn deriving (Eq, Show)
data Direction = Up | Down | All deriving (Eq, Show)


data Piece = Piece {
  getColor :: Color,
  getRole :: Role,
  getDirection :: Direction,
  hasMoved :: Bool
} deriving (Eq)


-- Use unicode symbols when printing.
instance Show Piece where
  show Piece { getColor = White, getRole = King } = "\x2654"
  show Piece { getColor = White, getRole = Queen } = "\x2655"
  show Piece { getColor = White, getRole = Bishop } = "\x2657"
  show Piece { getColor = White, getRole = Knight } = "\x2658"
  show Piece { getColor = White, getRole = Rook } = "\x2656"
  show Piece { getColor = White, getRole = Pawn } = "\x2659"
  show Piece { getColor = Black, getRole = King } = "\x265A"
  show Piece { getColor = Black, getRole = Queen } = "\x265B"
  show Piece { getColor = Black, getRole = Bishop } = "\x265D"
  show Piece { getColor = Black, getRole = Knight } = "\x265E"
  show Piece { getColor = Black, getRole = Rook } = "\x265C"
  show Piece { getColor = Black, getRole = Pawn } = "\x265F"


getOppositeColor :: Color -> Color
getOppositeColor White = Black
getOppositeColor Black = White