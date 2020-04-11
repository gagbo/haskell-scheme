-- | Main module

module Main where
import           System.Environment
import           HaskellScheme.Parser
import           HaskellScheme.Evaluator

main :: IO ()
main = getArgs >>= print . eval . readExpr . head
