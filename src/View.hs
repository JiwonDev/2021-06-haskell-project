module View
    ( drawState
    ) where

import Data.Maybe
import Graphics.Gloss hiding (Point)

import Direction
import GameBoard
import State

-- 블럭크기와 화면크기, 색상을 받아 사각형을 그림.
drawRect :: Float -> Float -> Float -> Float -> Color -> Point -> IO Picture
drawRect blockW blockH screenW screenH c (x, y) = do
    let x' = fromIntegral x
    let y' = fromIntegral y
    let transW = -screenW / 2 + (x' + 1) * blockW
    let transH = screenH / 2 - (y' + 1) * blockH
    return $ color c $ translate transW transH $ rectangleSolid blockW blockH

-- 리스트가 아닌경우 리스트로 만듬..
(?:) :: Maybe a -> [a] -> [a]
(?:) = maybe id (:)

-- 현재 상태를 그림.
drawState :: State -> IO Picture
drawState state = do
    blockW <- blockWidth
    blockH <- blockHeight
    screenW <- screenWidth
    screenH <- screenHeight
    target <- mapM (drawRect blockW blockH screenW screenH red) $ getTarget state
    player <- mapM (drawRect blockW blockH screenW screenH green) $ getPlayer state
    return $ pictures $ target ?: player
