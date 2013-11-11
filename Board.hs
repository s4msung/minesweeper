module Board
    ( Coord
    , Dimension
    , Square(..)
    , SquareMap
    , Overlay(..)
    , OverlayMap
    , Board(..)
    , createBoard
    , allCoords
    , flagOverlay
    , revealOverlay
    ) where

import           Data.List      (nub)
import qualified Data.Map       as M
import           System.Random  (mkStdGen, randomRs)

type Coord = (Int, Int)
type Dimension = (Int, Int)

data Square
    = Hint Int
    | Mine
    deriving (Show, Eq)

type SquareMap = M.Map Coord Square

data Overlay
    = Visible
    | Flag

type OverlayMap = M.Map Coord Overlay

data Board = Board
    { boardSize :: Dimension
    , boardSquares :: SquareMap
    , boardSeed :: Int
    }

createBoard :: Int -> Dimension -> Int -> Board
createBoard seed dim mines = Board
    { boardSize    = dim
    , boardSquares = addHints dim $ randomMines seed dim mines
    , boardSeed    = seed
    }

randomMines :: Int -> Dimension -> Int -> SquareMap
randomMines seed (width,height) mines = M.fromList $ map toCoord $ rMines mines (width*height)
    where
        rMines n nMax = take n $ nub $ randomRs (0,nMax-1) (mkStdGen seed)
        toCoord x = ((x `mod` width, x `div` width), Mine)

neighbours :: Dimension -> SquareMap -> Coord -> Int
neighbours dim smap coord = countNeighbours $ neighbourCoords dim coord
    where
        countNeighbours []     = 0
        countNeighbours (x:xs) = case M.lookup x smap of
            Just Mine -> 1 + countNeighbours xs
            _         -> countNeighbours xs

addHints :: Dimension -> SquareMap -> SquareMap
addHints dim smap = add $ filter ((> 0) . fst) $ zip (map (neighbours dim smap) allC) allC
    where
        allC = filter (`M.notMember` smap) $ allCoords dim
        add []                 = smap
        add ((hint, coord):xs) = M.insert coord (Hint hint) $ add xs

allCoords :: Dimension -> [Coord]
allCoords (width,height) = [ (x,y) | x <- [0..(width-1)], y <- [0..(height-1)]]

neighbourCoords :: Dimension -> Coord -> [Coord]
neighbourCoords (width,height) (i,j) =
    [ (x,y)
    | x <- [max 0 (i-1) .. min (width-1)  (i+1)]
    , y <- [max 0 (j-1) .. min (height-1) (j+1)]
    , (x,y) /= (i,j)
    ]

flagOverlay :: OverlayMap -> Coord -> OverlayMap
flagOverlay overlay coord = case M.lookup coord overlay of
    Nothing   -> M.insert coord Flag overlay
    Just Flag -> M.delete coord overlay
    _         -> overlay

revealOverlay :: Board -> OverlayMap -> Coord -> OverlayMap
revealOverlay board overlay coord
    | coord `M.member` overlay
        = overlay
    | otherwise
        = case M.lookup coord (boardSquares board) of
            Just Mine     -> overlay
            Just (Hint _) -> M.insert coord Visible overlay
            Nothing       -> foldr (M.insert `flip` Visible)
                                   overlay
                                   (emptyCoords board overlay coord)

emptyCoords :: Board -> OverlayMap -> Coord -> [Coord]
emptyCoords board overlay coord
    | isEmpty coord = emptyCoords' [coord] [coord]
    | otherwise     = []
  where
    emptyCoords' found [] = found
    emptyCoords' found (c:cs) =
        let unknown = filter isUnknown $
                             neighbourCoords (boardSize board) c
            reveal  = filter isEmpty unknown
            found'  = found ++ unknown
         in emptyCoords' found' (reveal ++ cs)
      where
        isUnknown c' = c' `notElem` found && c' `M.notMember` overlay
    isEmpty c' = c' `M.notMember` (boardSquares board)
