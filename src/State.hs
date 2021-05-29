module State
    ( State(..)
    , initState
    , setPlayer
    , setDirection
    , setTarget
    , setCounter
    , setPaused
    ) where

import Direction
import GameBoard
import Player

{-
* State = 플레이어, 방향, 타겟, 점수카운터, 게임종료여부
ex) `State [(0,0)] DOWN Nothing 0 False`
타겟은 없을 수도 있으므로 Maybe 로 작성

* State 내부 타입에
player :: Player로 이름을 정할 경우
아래 메서드의 정의가 헷갈려서
player -> getPlayer로 정의함.

* 각 옵션에 따른 setter 함수 정의.
-}
data State =
    State
        { getPlayer :: Player
        , getDirection :: Direction
        , getTarget :: Maybe Point
        , getCounter :: Int
        , getPaused :: Bool
        }

-- 초기상태
initState :: State
initState = State [(0, 0)] DOWN Nothing 0 False

-- 상태 변경
setPlayer :: Player -> State -> State
setPlayer player state = state {getPlayer = player}

-- 방향 변경
setDirection :: Direction -> State -> State
setDirection dir state = state {getDirection = dir}


-- 목표(Target) 변경
setTarget :: Maybe Point -> State -> State
setTarget target state = state {getTarget = target}


-- 점수 카운터 변경
setCounter :: Int -> State -> State
setCounter counter state = state {getCounter = counter}


-- 정지
setPaused :: Bool -> State -> State
setPaused paused state = state {getPaused = paused}
