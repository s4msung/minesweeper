module Main where

import Control.Monad.IO.Class  (MonadIO)
import Data.IORef              (IORef, readIORef, modifyIORef)
import Haste
import Haste.Graphics.Canvas   (getCanvas)
import Board                   (Coord, flagOverlay, revealOverlay)
import State                   (GameState(..), initialGameState, stateBoardSize)
import Widget                  (label, inputNumber, inputNumberR, button, gameCanvas)
import Render                  (renderGameState, renderSquare, renderFocus)

preferences :: MonadIO m => m Elem
preferences = do
    title <- newElem "h2"
    setProp title "innerHTML" "Schwierigkeitsgrad ausw&auml;hlen"

    labelSeed <- label "seed" "Startwert (optional)"
    inputSeed <- inputNumber "seed" ""

    btn1 <- button "Anf&auml;nger (10 Minen; 9x9 Spielfeldraster)"
    btn2 <- button "Fortgeschritten (40 Minen; 16x16 Spielfeldraster)"
    btn3 <- button "Profi (99 Minen; 30x16 Spielfeldraster)"


    labelHeight <- label "height" "H&ouml;he"
    inputHeight <- inputNumberR "height" "9" "9" "24"

    labelWidth <- label "width" "Breite"
    inputWidth <- inputNumberR "width" "9" "9" "30"

    labelMines <- label "mines" "Minen"
    inputMines <- inputNumberR "mines" "10" "10" "668"

    btn4 <- button "Benutzerdefiniert"

    seed <- newElem "div"
    setChildren seed [labelSeed, inputSeed]

    def <- newElem "div"
    setChildren def [btn1, btn2, btn3]

    custom <- newElem "div"
    setChildren custom [labelWidth, inputWidth, labelHeight, inputHeight, labelMines, inputMines, btn4]
    

    pref <- newElem "div"
    setChildren pref [title, seed, def, custom]

    _ <- btn1 `onEvent` OnClick $ \_btn _p -> do 
        mSeed   <- getValue inputSeed
        case mSeed of
            Just s -> initialGameState s (9,9) 10 >>= startGame
            _      -> return ()
    _ <- btn2 `onEvent` OnClick $ \_btn _p -> do
        mSeed   <- getValue inputSeed
        case mSeed of
            Just s -> initialGameState s (16,16) 40 >>= startGame
            _      -> return ()
    _ <- btn3 `onEvent` OnClick $ \_btn _p -> do
        mSeed   <- getValue inputSeed
        case mSeed of
            Just s -> initialGameState s (30,16) 99 >>= startGame
            _      -> return ()
    _ <- btn4 `onEvent` OnClick $ \_btn _p -> do
        mSeed   <- getValue inputSeed
        mWidth  <- getValue inputWidth
        mHeight <- getValue inputHeight
        mMines  <- getValue inputMines
        case (mSeed, mWidth, mHeight, mMines) of
            (Just s, Just w, Just h, Just m) -> initialGameState s (w,h) m >>= startGame
            _                                -> return ()

    return pref

pointToCoord :: (Int,Int) -> Coord
pointToCoord (x,y) = (x `quot` 16, y `quot` 16)

startGame :: IORef GameState -> IO ()
startGame state = do
    firstState <- readIORef state
    gameCanvas' <- gameCanvas $ stateBoardSize firstState
    withElem "game" $ \e -> setChildren e [gameCanvas']

    Just c <- getCanvas gameCanvas'

    _ <- gameCanvas' `onEvent` OnMouseOut $ do
        readIORef state >>= \st -> do
            case (stateFocus st) of
                Just coord -> renderSquare c coord st
                Nothing    -> return ()
        modifyIORef state $ \st -> st {stateFocus = Nothing}

    _ <- gameCanvas' `onEvent` OnMouseMove $ \p -> do
        readIORef state >>= \st -> do
            case (stateFocus st) of
                Just coord -> renderSquare c coord st
                Nothing    -> return ()
        modifyIORef state $ \st -> st {stateFocus = Just $ pointToCoord p}
        readIORef state >>= renderFocus c

    _ <- gameCanvas' `onEvent` OnClick $ \btn p -> do
        let coord = pointToCoord p
        if btn /= 0
            then modifyIORef state $ \st -> st {stateOverlay = flagOverlay (stateOverlay st) coord}
            else modifyIORef state $ \st -> do
                st {stateOverlay = revealOverlay (stateBoard st) (stateOverlay st) coord}
        readIORef state >>= renderGameState c

    readIORef state >>= renderGameState c

main :: IO ()
main = withElem "game" $ \e -> do
    pref <- preferences
    setChildren e [pref]
