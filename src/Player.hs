module Player
    ( Player
    , valid
    , move
    , eating
    , eat
    ) where

import Data.List
import Direction

type Player = [Point]

-- 플레이어 움직이기
move :: Player -> Direction -> Player
move player dir = push dir (head player) : init player

-- 꼬리 추가
eat :: Player -> Direction -> Player
eat player dir = push dir (head player) : player

-- 해당 좌표가 스크린 안쪽인지 판단.
insideScreen :: Int -> Int -> Point -> Bool
insideScreen xMax yMax (x, y) = x >= 0 && y >= 0 && x < xMax && y < yMax

-- 꼬리에 다였는지 (겹치는 지) 판단
noHitTail :: Player -> Bool
noHitTail (x:xs) = not $ elem x xs

-- 플레이어가 화면 내부에 있는지 판단.
valid :: Int -> Int -> Player -> Bool
valid xMax yMax player = noHitTail player && insideScreen xMax yMax (head player)

-- 목표를 먹었을떄 확인.
-- eating player Target = (head player) == Target
eating :: Player -> Point -> Bool
eating = (==) . head
