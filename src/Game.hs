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

-- 목표(Target)가 없는 경우 랜덤으로 생성
genTarget :: State -> IO State
genTarget state =
    case getTarget state of
        Just _ -> return state
        Nothing -> do
            x <- randomRIO (0, gridWidth - 1)
            y <- randomRIO (0, gridHeight - 1)
            if (x, y) `elem` getPlayer state
                then do -- 플레이어가 죽은 경우
                    putStrLn "죽었습니다.."
                    genTarget state
                else do
                    return $ setTarget (Just (x, y)) state

-- 플레이어(뱀) 움직이기
doMove :: State -> State
doMove state =
    case getCounter state of
        0 ->
            let newPlayer = move (getPlayer state) (getDirection state)
             in setPlayer newPlayer state
        c ->
            let newPlayer = eat (getPlayer state) (getDirection state)
             in setCounter (c - 1) $ setPlayer newPlayer state

-- 플레이어가 음식을 먹었을 때
eatTarget :: State -> State
eatTarget state =
    case getTarget state of
        Nothing -> state
        (Just f) ->
            if getPlayer state `eating` f
                then let newCounter = getCounter state + 5
                      in setTarget Nothing $ setCounter newCounter state
                else state

-- 게임 진행 (한 프레임)
step :: Float -> State -> IO State
step _ state = do
    when (invalid state) (exitGame state)
    if getPaused state
        then return state
        else step' state
  where
    step' = genTarget . eatTarget . doMove
    invalid = not . valid gridWidth gridHeight . getPlayer

-- 게임을 종료하고 score 를 print 함.
exitGame :: State -> IO ()
exitGame state = do
    let score = (length $ getPlayer state) - 1
    putStrLn $ "당신의 점수는 :: " ++ show score
    putStrLn "플레이 해주셔서 감사합니다."
    exitSuccess

-- 게임 시작
screen :: Display
screen = InWindow "2015642028 김지원" (800,800) (0, 0) -- 이름 , size , position

background:: Color
background = black

game :: IO ()
{- Graphics.Gloss.Interface.IO.Game playIO 메서드
  https://hackage.haskell.org/package/gloss-1.13.2.1/docs/Graphics-Gloss-Interface-IO-Game.html
  playIO
  -> displayMode
  -> displayColor
  -> frame(1초당 step 수)
  -> world(게임 init)
  -> (word -> IO Picture) 게임 View
  -> (Event -> world -> IO world) 이벤트 핸들러
  -> (Float -> world -> IO world) 프레임 단위로 실행할 함수 (step)
-}

game = playIO screen background 10 initState drawState handleInput step












