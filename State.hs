module State
    ( GameState(..)
    , initialGameState
    , stateBoardSize
    , stateBoardSquares
    ) where

import           Board       (Board(..), SquareMap, OverlayMap, Coord, Dimension, createBoard)
import qualified Data.Map    as M
import           Data.IORef  (IORef, newIORef)

data GameState = GameState
    { stateBoard :: Board
    , stateOverlay :: OverlayMap
    , stateFocus :: Maybe Coord
    }

initialGameState :: Int -> Dimension -> Int -> IO (IORef GameState)
initialGameState seed dimension mines = newIORef $ GameState
    { stateBoard          = createBoard seed dimension mines
    , stateOverlay        = M.empty
    , stateFocus          = Nothing
    }

stateBoardSize :: GameState -> Dimension
stateBoardSize = boardSize . stateBoard

stateBoardSquares :: GameState -> SquareMap
stateBoardSquares = boardSquares . stateBoard
