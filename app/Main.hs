{- 메인코드 작성 -}
main :: IO ()
main = do
    putStrLn "Hello, world!"
    putStrLn "Hello, what's your name?"
    name <- getLine
    putStrLn ("Hey " ++ name ++ "!")
