module Direction
    ( Direction(..)
    , Point
    , push
    , opposite
    ) where

import Data.Bifunctor

-- Direction 데이터 타입 정의
data Direction
    = UP
    | DOWN
    | LEFT
    | RIGHT
    deriving (Eq)

-- Point 타입 정의
type Point = (Int, Int)

{- 
-- 방향 -> 좌표 반환 (Bifunctor 를 이용하여 2개의 인자를 하나로 묶어사용)
-- Enum (pred, succ)
-- bimap f g ≡ first f . second g
-- (pred,pred) = 왼쪽위, (pred,succ) = 왼쪽아래, (succ, pred) = 오른쪽위, (succ,succ) = 오른쪽아래
-}
push :: Direction -> Point -> Point
push UP = second pred
push DOWN = second succ
push LEFT = first pred
push RIGHT = first succ

-- 방향 반전 함수
opposite :: Direction -> Direction
opposite UP = DOWN
opposite DOWN = UP
opposite LEFT = RIGHT
opposite RIGHT = LEFT
