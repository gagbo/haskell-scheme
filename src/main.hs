-- | Main module

module Main where
import           System.Environment
import           HaskellScheme.Parser
import           HaskellScheme.Evaluator()

main :: IO ()
main = do
  (expr : _) <- getArgs
  putStrLn $ case (readExpr expr) of
    Left err -> err
    Right val -> show val
