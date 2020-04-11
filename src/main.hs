-- | Main module

module Main where
import           System.Environment
import           HaskellScheme.Parser
import           HaskellScheme.Evaluator
import           HaskellScheme.LispTypes
import           Control.Monad.Except

main :: IO ()
main = do
  args   <- getArgs
  evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
  putStrLn $ extractValue $ trapError evaled
