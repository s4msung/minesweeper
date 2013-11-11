module Render
    ( renderGameState
    , renderSquare
    , renderFocus
    ) where

import qualified Data.Map                as M
import           Haste                   (convert)
import           Haste.Graphics.Canvas   (Canvas, Picture, render, render', Color(..), color, fill, rect, text)
import           Board                   (Square(..), SquareMap, Overlay(..), OverlayMap, Coord, allCoords)
import           State                   (GameState(..), stateBoardSquares, stateBoardSize)

drawSquare :: SquareMap -> OverlayMap -> Coord -> Picture ()
drawSquare s o coord@(i,j) = case M.lookup coord o of
    Nothing       -> color (RGB 100 100 100) . fill $ rect (x, y) (x+16, y+16)
    Just Flag     -> color (RGB 100 255 100) . fill $ rect (x, y) (x+16, y+16)
    Just Visible  -> case M.lookup coord s of
        Nothing       -> color (RGB 220 220 220) . fill $ rect (x, y) (x+16, y+16)
        Just Mine     -> color (RGB 255   0   0) . fill $ rect (x, y) (x+16, y+16)
        Just (Hint h) -> do
            color (RGB 150 150 255) . fill $ rect (x, y) (x+16, y+16)
            color (RGB   0   0   0) . text (x+6, y+12) $ show h
    where
    (x,y) = (convert $ i*16, convert $ j*16)

drawFocus :: Maybe Coord -> Picture ()
drawFocus focus = case focus of
    Nothing     -> return ()
    Just (x, y) -> let (x',y') = (convert $ x*16, convert $ y*16)
                   in color (RGBA 255 255 255 0.5) . fill $ rect (x', y') (x'+16, y'+16)

renderGameState :: Canvas -> GameState -> IO ()
renderGameState c state = render c $ do
    mapM_ (drawSquare (stateBoardSquares state) (stateOverlay state)) $ allCoords (stateBoardSize state)
    drawFocus $ stateFocus state

renderFocus :: Canvas -> GameState -> IO ()
renderFocus c state = render' c $ drawFocus (stateFocus state)

renderSquare :: Canvas -> Coord -> GameState -> IO ()
renderSquare c coord state = render' c $ drawSquare (stateBoardSquares state) (stateOverlay state) coord
