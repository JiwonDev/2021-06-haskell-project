module Player
    ( Player
    , valid
    , move
    , eating
    , eat
    ) where

import Data.List
import Direction
{-
Player.hs 작성
플레이어를 만들고 플레이어의 좌표값을 정의함.
플레이어 관련 함수 (move, eat valid eating)
게임 종료 조건 함수 (noHitTail insideScreen)
-}
-- Player는 좌표로 이루어진 배열임. Player 배열에 저장된 값을 화면에 그림.
type Player = [Point]

-- 플레이어 움직이기. 방향과 player를 주면 player의 마지막값 (꼬리)를 지우고 앞의 값을 추가함.
move :: Player -> Direction -> Player
move player dir = push dir (head player) : init player -- [플레이어]의 마지막값을 제외한 나머지

-- 해당 방향으로 꼬리 추가. move와 동일하나 마지막값을 지우지 않음.
eat :: Player -> Direction -> Player
eat player dir = push dir (head player) : player

-- 해당 좌표가 스크린 안쪽인지 판단.
insideScreen :: Int -> Int -> Point -> Bool
insideScreen xMax yMax (x, y) = x >= 0 && y >= 0 && x < xMax && y < yMax

-- 해당 좌표가 Player와 겹치는지 판단
noHitTail :: Player -> Bool
noHitTail (x:xs) = not $ elem x xs

-- 플레이어가 화면 내부에 있는지 판단.
valid :: Int -> Int -> Player -> Bool
valid xMax yMax player = noHitTail player && insideScreen xMax yMax (head player)

-- 목표를 먹었을때, 플레이어의 첫번째값 (좌표)가 Target과 동일한지 확인 후 bool 반환
-- eating player Target = (head player) == Target
eating :: Player -> Point -> Bool
eating = (==) . head
