module Paths (
  Path,
  getAllDiagonalPaths,
  getAllStraightPaths,
  getPathN,
  getPathNE,
  getPathE,
  getPathSE,
  getPathS,
  getPathSW,
  getPathW,
  getPathNW
) where

import Data.Char (ord, chr)
import Board
import Piece


-- A path is a series of space ids that represent the path a piece
-- can take when chosing a move. The id at index 0 corresponds to
-- a space adjacent to the current piece, while the last item in
-- the path will be at the edge of the board.
type Path = [ SpaceId ]


-- The following functions all return paths going in different directions
-- from the given space id. 'N' corresponds to all spaces above the given
-- piece (when printed to the screen), and so on.


getPathNE :: SpaceId -> Path
getPathNE = 
  let getNextId [ col, row ] = [ increment 1 col, increment 1 row ]
  in getDiagonalPath getNextId


getPathNW :: SpaceId -> Path
getPathNW = 
  let getNextId [ col,row ] = [ decrement 1 col, increment 1 row ]
  in getDiagonalPath getNextId


getPathSW :: SpaceId -> Path
getPathSW = 
  let getNextId [ col, row ] = [ decrement 1 col, decrement 1 row ]
  in getDiagonalPath getNextId


getPathSE :: SpaceId -> Path
getPathSE = 
  let getNextId [ col, row ] = [ increment 1 col, decrement 1 row ]
  in getDiagonalPath getNextId


getPathN :: SpaceId -> Path
getPathN [ col, row ] = 
  reverse . map (\r -> [col, r]) $ takeWhile (\r -> ord r > ord row) ['8'..'1']


getPathS :: SpaceId -> Path
getPathS [ col, row ] =
  reverse . map (\r -> [col, r]) $ takeWhile (\r -> ord r < ord row) ['1'..'8']


getPathW :: SpaceId -> Path
getPathW [ col, row ] =
  reverse . map (\c -> [c, row]) $ takeWhile (\c -> ord c < ord col) ['a'..'h']


getPathE :: SpaceId -> Path
getPathE [ col, row ] =
  reverse . map (\c -> [c, row]) $ takeWhile (\c -> ord c > ord col) ['h'..'i']


-- These return arrays of paths so that each path can be individually
-- mapped before being used.


-- Return array of paths going in all diagonal directions. e.g., NE, NW, etc.
getAllDiagonalPaths :: SpaceId -> [ Path ]
getAllDiagonalPaths id =
  map ($id) [ getPathNE, getPathNW, getPathSW, getPathSE ]


-- Return array of paths going in all straight directions. e.g., N, S, etc.
getAllStraightPaths :: SpaceId -> [ Path ]
getAllStraightPaths id =
  map ($id) [ getPathN, getPathW, getPathS, getPathE ]


-- Helpers 


-- Given a function that determines how to calculate the next space id
-- on the path, build up a diagonal path.
getDiagonalPath :: (SpaceId -> SpaceId) -> SpaceId -> Path
getDiagonalPath getNextId id =
  if not $ isOnBoard id
  then []
  else let nextId = getNextId id in nextId : getDiagonalPath getNextId nextId