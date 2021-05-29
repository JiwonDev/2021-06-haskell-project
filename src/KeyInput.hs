{-# LANGUAGE PatternSynonyms #-}

module KeyInput
    ( handleInput
    ) where

import Data.Maybe
import Graphics.Gloss.Interface.IO.Game hiding (Point)
import System.Exit

import Direction
import State

pattern KeyHeld k <- EventKey k Down x y

pattern SKeyHeld k <- KeyHeld (SpecialKey k)

-- Handle input
-- exitSuccess 는 시스템 종료 (== System.Exit)
handleInput :: Event -> State -> IO State
handleInput (SKeyHeld KeyEsc) _ = exitSuccess
handleInput (SKeyHeld KeySpace) state =
    return $ setPaused (not $ getPaused state) state
handleInput (KeyHeld k) state
    | getPaused state = return state
    | otherwise = return $ maybe id trySetDirection (keyToDir k) state
handleInput _ state = return state

-- 키 입력 -> 방향
-- Maybe(Just nothing)을 이용하여 null 값처리
keyToDir :: Key -> Maybe Direction
keyToDir (Char 'w') = Just UP
keyToDir (Char 'a') = Just LEFT
keyToDir (Char 's') = Just DOWN
keyToDir (Char 'd') = Just RIGHT
keyToDir (SpecialKey KeyUp) = Just UP
keyToDir (SpecialKey KeyLeft) = Just LEFT
keyToDir (SpecialKey KeyDown) = Just DOWN
keyToDir (SpecialKey KeyRight) = Just RIGHT
keyToDir _ = Nothing

-- 플레이어 방향 설정
trySetDirection :: Direction -> State -> State
trySetDirection dir state =
    case getPlayer state of
        [x] -> setDirection dir state
        (x:y:_) ->
            if push dir x == y
                then state
                else setDirection dir state
