module GameBoard where

import Control.Monad
{-
GameBoard.hs 작성
스크린 사이즈를 이용하여 block, grid(게임판), screen 관련 함수 정의
-}
-- 그리드 가로 사이즈 (개수)
gridWidth :: Num a => a
gridWidth = 40

-- 그리드 세로 사이즈
gridHeight :: Num a => a
gridHeight = 30

-- 만약 전체화면(fullScreen) 으로 실행하고싶다면
-- Graphics.Gloss.Interface.Environment 에 있는 getScreenSize 이용
-- fst getScreenSize, snd getScreenSize
getScreenSize :: IO (Int, Int)
getScreenSize = return (800,  600)

-- 픽셀의 가로 갯수 (스크린 내에서)
screenWidth :: Num a => IO a
screenWidth = fromIntegral . fst <$> getScreenSize

-- 픽셀의 세로 갯수 (스크린 내에서)
screenHeight :: Num a => IO a
screenHeight = fromIntegral . snd <$> getScreenSize

-- 총 가로 블럭 갯수, 블럭크기 / 스크린 사이즈
blockWidth :: IO Float
blockWidth = (/ (gridWidth + 1)) <$> screenWidth

-- 총 세로 블럭 갯수
blockHeight :: IO Float
blockHeight = (/ (gridHeight + 1)) <$> screenHeight
