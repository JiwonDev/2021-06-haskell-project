module Game where

import Control.Monad
import Data.Maybe (isNothing)
import Direction
import View

import Graphics.Gloss hiding (Point)
import Graphics.Gloss.Interface.Environment
import Graphics.Gloss.Interface.IO.Game hiding (Point)
import GameBoard
import KeyInput

import Player
import State
import System.Exit
import System.Random

{- Graphics.Gloss.Interface.IO.Game playIO 메서드
  https://hackage.haskell.org/package/gloss-1.13.2.1/docs/Graphics-Gloss-Interface-IO-Game.html
  game playIO
  -> displayMode
  -> displayColor
  -> frame(1초당 step 수)
  -> world(게임 init)
  -> (word -> IO Picture) 게임 View
  -> (Event -> world -> IO world) 이벤트 핸들러
  -> (Float -> world -> IO world) 프레임 단위로 실행할 함수 (step)

  genTarget :: 현재 state 를 받아 목표(Target)을 생성. player state가 죽은 경우 게임 종료.

-}

-- 목표(Target)가 없는 경우 랜덤으로 생성
genTarget :: State -> IO State
genTarget state =
    case getTarget state of
        Just _ -> return state
        Nothing -> do -- getTarget의 반환값이 없는 경우 (nothing)
            x <- randomRIO (0, gridWidth - 1) -- random 메서드로 랜덤한 좌표 생성
            y <- randomRIO (0, gridHeight - 1)
            if (x, y) `elem` getPlayer state -- 현재 플레이어의 위치와 겹치는지 확인
                then do -- 겹친다면 다시 현재 함수를 다시 실행
                    genTarget state
                else do -- 겹치지 않는다면 Target의 State를 변경하고 생성
                    return $ setTarget (Just (x, y)) state

-- 플레이어 움직이기. 각 step 마다 반복
doMove :: State -> State
doMove state =
    case getCounter state of -- 현재 State에서 Counter를 받아옴.
        0 -> -- count 가 0인 경우 Player.move 함수로 캐릭터를 움직임.
            let newPlayer = move (getPlayer state) (getDirection state)
             in setPlayer newPlayer state
        c -> -- count 가 0이 아닌경우 Player.eat 함수로 꼬리를 1칸 늘림 (c-1)
            let newPlayer = eat (getPlayer state) (getDirection state)
             in setCounter (c - 1) $ setPlayer newPlayer state

-- 플레이어가 음식을 먹었을 때
eatTarget :: State -> State
eatTarget state =
    case getTarget state of
        Nothing -> state -- target이 없는경우 현재 state를 반환
        (Just f) -> -- target이 있는 경우
            if getPlayer state `eating` f -- Player.eating은 현재 플레이어와 Target이 같은 위치인지 반환(True,False)
                then let newCounter = getCounter state + 1 -- counter값을 늘림. doMove에서 카운터 값만큼 꼬리가 추가됨
                      in setTarget Nothing $ setCounter newCounter state
                else state

-- 게임 진행 (한 프레임)
-- step 구현은 잘 몰라서 다음 코드를 참고하였음.
-- https://github.com/Jonathas-Conceicao/Snake-Haskell
step :: Float -> State -> IO State
step _ state = do
  -- when 모나드, Monad a => Bool -> a() -> a()
  -- invalid가 True 라면 exitGame를 실행, False 라면 두번쨰 메서드를 실행함.
    when (invalid state) (exitGame state) -- when 모나드
    if getPaused state -- Pause 가 True 인 경우
        then return state -- 현재 State를 반환하고 아무것도 하지않음.
        else step' state -- False면 다음 Step 실행
  where
    step' = genTarget . eatTarget . doMove -- Target 생성 -> Target 겹치는지 여부 확인 -> 플레이어 움직임.
    invalid = not . valid gridWidth gridHeight . getPlayer -- player값을 받아와 밖으로 나갔는지(grid) 확인

-- 게임을 종료하고 score 를 print 함.
exitGame :: State -> IO ()
exitGame state = do
    let score = (length $ getPlayer state) - 1
    putStrLn $ "당신의 점수는 :: " ++ show score
    putStrLn "플레이 해주셔서 감사합니다."
    exitSuccess -- System.Exit, 프로그램 종료

-- 게임 시작
screen :: Display -- 전체화면으로 하고 싶다면 gloss 라이브러리에 내장된 fullscreen 사용.
screen = InWindow "2015642028 김지원" (800,600) (0, 0) -- 이름 , size , position

background:: Color
background = black

game :: IO ()
game = playIO screen background 20 initState drawState handleInput step












