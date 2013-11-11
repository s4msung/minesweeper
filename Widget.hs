module Widget
    ( label
    , button
    , inputNumber
    , inputNumberR
    , gameCanvas
    ) where

import           Control.Monad.IO.Class  (MonadIO)
import           Haste
import           Board                   (Dimension)

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

gameCanvas :: MonadIO m => Dimension -> m Elem
gameCanvas (width, height) = do
    canvas <- newElem "canvas"
    setProp canvas "id"     "minemap"
    setProp canvas "width"  (show (width*16))
    setProp canvas "height" (show (height*16))

    return canvas

