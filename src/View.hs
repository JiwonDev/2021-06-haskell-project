module View
    ( drawState
    ) where

import Data.Maybe
import Graphics.Gloss hiding (Point)

import Direction
import GameBoard
import State
{-
PlayIO 에 인자로 넘길 View.hs 작성
- drawRect 사각형을 그림
- drawState - drawRect을 이용하여 플레이어와 타겟을 그림
-}
-- 블럭크기와 화면크기, 색상을 받아 사각형을 그림.
drawRect :: Float -> Float -> Float -> Float -> Color -> Point -> IO Picture
drawRect blockW blockH screenW screenH c (x, y) = do
    let x' = fromIntegral x
    let y' = fromIntegral y
    let transW = -screenW / 2 + (x' + 1) * blockW
    let transH = screenH / 2 - (y' + 1) * blockH
    return $ color c $ translate transW transH $ rectangleSolid blockW blockH

-- 값과 리스트를 주면 하나의 리스트 ( target:[player] )로 만들어 반환함.
-- 예제 코드에서 이런식으로 사용하는 방법도 있길래 그대로 사용. 사실 잘 모르겠음
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
    -- Graphics.Gloss.Data.Picture 의 pictures에 그릴 좌표들을 mapM에 담아 반환함.
    return $ pictures $ target ?: player
