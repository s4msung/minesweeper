module Main where

import Control.Monad.IO.Class  (MonadIO(..))
import Data.IORef
import qualified Data.Map as M
import Haste
import Haste.Graphics.Canvas
import Board

data GameState = GameState
    { stateBoard :: Board
    , stateOverlay :: OverlayMap
    , stateFocus :: Maybe Coord
    }

label :: MonadIO m => String -> String -> m Elem
label for content = do
    l <- newElem "label"
    setProp l "for" for
    setProp l "innerHTML" content
    return l

button :: MonadIO m => String -> m Elem
button content = do
    b <- newElem "button"
    setProp b "innerHTML" content
    return b

inputNumber :: MonadIO m => String -> String -> m Elem
inputNumber identifier value = do
    i <- newElem "input"
    setProp i "id"     identifier
    setProp i "type"   "number"
    setProp i "value"  value
    return i

inputNumberR :: MonadIO m => String -> String -> String -> String -> m Elem
inputNumberR identifier value start end = do
    i <- inputNumber identifier value
    setProp i "min" start
    setProp i "max" end
    return i

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
        {-mSeed   <- getValue inputSeed-}
        initialGameState 1024 (9,9) 10 >>= game
    _ <- btn2 `onEvent` OnClick $ \_btn _p -> do
        {-mSeed   <- getValue inputSeed-}
        initialGameState 1024 (16,16) 40 >>= game
    _ <- btn3 `onEvent` OnClick $ \_btn _p -> do
        {-mSeed   <- getValue inputSeed-}
        initialGameState 1024 (30,16) 99 >>= game
    _ <- btn4 `onEvent` OnClick $ \_btn _p -> do
        {-mSeed   <- getValue inputSeed-}
        mWidth  <- getValue inputWidth
        mHeight <- getValue inputHeight
        mMines  <- getValue inputMines
        case (mWidth, mHeight, mMines) of
            (Just w, Just h, Just m) -> initialGameState 1024 (w,h) m >>= game
            _                        -> return ()

    return pref

initialGameState :: Int -> Dimension -> Int -> IO (IORef GameState)
initialGameState seed dimension mines = newIORef $ GameState
    { stateBoard          = createBoard seed dimension mines
    , stateOverlay        = M.empty
    , stateFocus          = Nothing
    }

gameCanvas :: MonadIO m => Int -> Int -> m Elem
gameCanvas width height = do
    canvas <- newElem "canvas"
    setProp canvas "id"     "minemap"
    setProp canvas "width"  (show width)
    setProp canvas "height" (show height)

    return canvas

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
    x = convert $ i*16
    y = convert $ j*16

drawGameState :: Canvas -> GameState -> IO ()
drawGameState c state = do
    render c $ do
        let b = stateBoard state
        mapM_ (drawSquare ((boardSquares . stateBoard) state) (stateOverlay state)) $ allCoords (boardSize b)
        drawFocus $ stateFocus state

drawFocus :: Maybe Coord -> Picture ()
drawFocus focus = case focus of
    Nothing     -> return ()
    Just (x, y) -> do
        let x' = convert $ x*16
            y' = convert $ y*16
        color (RGBA 255 255 255 0.5) . fill $ rect (x', y') (x'+16, y'+16)

pointToCoord :: (Int,Int) -> Coord
pointToCoord (x,y) = (x `quot` 16, y `quot` 16)

game :: IORef GameState -> IO ()
game state = do

    firstState <- readIORef state
    let (width,height) = (boardSize . stateBoard) firstState
    gameCanvas' <- gameCanvas (width*16) (height*16)
    withElem "game" $ \e -> setChildren e [gameCanvas']

    Just c <- getCanvas gameCanvas'

    _ <- gameCanvas' `onEvent` OnMouseOut $ do
        readIORef state >>= \st -> do
            case (stateFocus st) of
                Just coord -> render' c $ drawSquare ((boardSquares . stateBoard) st) (stateOverlay st) coord
                Nothing -> return ()
        modifyIORef state $ \st -> st {stateFocus = Nothing}

    _ <- gameCanvas' `onEvent` OnMouseMove $ \p -> do
        readIORef state >>= \st -> do
            case (stateFocus st) of
                Just coord -> render' c $ drawSquare ((boardSquares . stateBoard) st) (stateOverlay st) coord
                Nothing -> return ()
        modifyIORef state $ \st -> st {stateFocus = Just $ pointToCoord p}
        readIORef state >>= \st -> do
            render' c $ drawFocus (stateFocus st)

    _ <- gameCanvas' `onEvent` OnClick $ \btn p -> do
        let coord = pointToCoord p
        if btn /= 0
            then modifyIORef state $ \st -> st {stateOverlay = flagOverlay (stateOverlay st) coord}
            else modifyIORef state $ \st -> do
                st {stateOverlay = revealOverlay (stateBoard st) (stateOverlay st) coord}
        readIORef state >>= drawGameState c

    readIORef state >>= drawGameState c

    return ()

main :: IO ()
main = withElem "game" $ \e -> do
    pref <- preferences
    setChildren e [pref]

