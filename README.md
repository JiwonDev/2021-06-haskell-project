# haskell-project
고급프로그래밍 프로젝트 과제

## 프로젝트 빌드&실행
프로젝트 setup 코드는 따로 작성한게 없어서 안해서 생략해도 상관없긴함.

프로젝트 실행이 안되면 기존 빌드 파일 (haskell-project.cabel) 삭제 후 package.yaml 수정 후 다시 빌드

![Imgur](https://i.imgur.com/uOKu2F2.png)

```github
git clone
stack setup
stack build
stack exec jiwon
```
## 프로젝트 개요
하스켈 gloss 그래픽 모듈을 이용한 간단한 꼬리잡기 게임.
[Graphics.Gloss.Interface.IO.Game]((https://hackage.haskell.org/package/gloss-1.13.2.1/docs/Graphics-Gloss-Interface-IO-Game.html)) 을 사용하여 구현하였음.


참고로 stack 을 이용하여 gloss 설치시 필수 dll 파일이 없어서 실행되지 않음. 따로 설치해주어야함
( [issues #1](https://github.com/JiwonDev/haskell-project/issues/1, "gloss 라이브러리 빌드 시 glut.dll 관련 에러 (user error)")
)

```haskell
game :: IO ()
game = playIO screen background 20 initState drawState handleInput step

{-
    -Graphics.Gloss.Interface.IO.Game playIO 메서드
    game = playIO <입력값 7개>
    
    입력값1 < Game.hs/screen > 
        :: displayMode, 프로그램 윈도우 크기
        
    입력값2 < Game.hs/background > 
        :: displayColor, 디스플레이 기본 색상
        
    입력값3 < 20 >
        :: frame, 초당 새로고침 할 횟수  (step 동작 횟수)
        
    입력값4 < State.hs/initState > 
        :: world 초기 데이터 정보 (게임 init)
        
    입력값5 < View.hs/drawState > 
        :: (word -> IO Picture) 화면 출력 함수
        
    입력값6 < KeyInput.hs/handleInput > 
        :: (Event -> world -> IO world) 키 입력 및 이벤트 핸들러
        
    입력값7 < Game.hs/step > 
        :: (Float -> world -> IO world) 프레임 단위로 실행할 함수
-}

```

## 프로젝트 구조
```haskell
.idea/ -- 편집기 설정파일
.stack-word / -- stack build 완료된 파일 (스냅샷 관리)
app/Main.hs -- 프로젝트 진입점

src// {- 소스코드 -}
    src/Directions.hs -- 좌표값, 방향 데이터 타입 및 함수
    src/GameBoard.hs -- 게임 내 블럭, 화면, 실제 윈도우 크기 데이터
    src/Player.hs -- 플레이어 정의 및 동작 관련 함수
    src/KeyInput.hs -- 키보드 입력 정의, PlayIO의 이벤트 핸들러 함수 (handleInput)
    src/View.hs -- 화면 출력. PlayIO의 화면 출력 함수(drawState)
    src/State.hs -- 게임 State 정의 및 State 변경 함수. PlayIO의 world 함수
        -- State 타입은 아래와 같으며 PlayIO 내부에 저장됨.
        -- PlayIO에 저장된 State는 drawState에 전달됨.
        data State =
            State
                { getPlayer :: Player
                , getDirection :: Direction
                , getTarget :: Maybe Point
                , getCounter :: Int
                , getPaused :: Bool
                }
```
```alex
haskell-project.cabel -- 빌드된 프로젝트 설정파일 (자동생성)
haskell-project.iml -- 빌드된 프로젝트 설정파일 (자동생성)
package.yaml -- 프로젝트 설정파일
stack.yaml -- stack 설정파일 
setup.hs -- 프로젝트 빌드 전 stack setup 용. 이 프로젝트에서는 사용하지 않음.
test/ -- test app 진입점. package.yaml 에서 설정 할 수 있음.
```

## 만드는데 도움이 된 Reference

https://github.com/tfausak/haskell-snake-game

https://hackage.haskell.org/package/gloss-1.13.2.1/docs/Graphics-Gloss-Interface-IO-Game.html

https://github.com/DarkKnight2000/InfinityLoop

https://hoogle.haskell.org/

https://github.com/Jonathas-Conceicao/Snake-Haskell
