-- | Main module

module Main where
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    putStrLn("What's your name ?")
    name <- getLine
    putStrLn ("Hello, " ++ name)
    putStrLn (show $ (read $ args !! 0) + (read $ args !! 1))
