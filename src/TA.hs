module Main where

import TAEval
import TAType
import TACheck
import TAParser
import TAPretty
import CalcSyntax

import Data.Maybe

import Control.Monad.Trans
import System.Console.Haskeline

eval' :: Expr -> Expr
eval' = fromJust . eval

process :: String -> IO ()
process line = do
  let res = parseExpr line
  case res of
    Left err -> print err
    Right ex -> do
      let chk = check ex
      case chk of
        Left err -> print err
        Right ty -> putStrLn $ (ppexpr $ eval' ex) ++ " : " ++ (pptype ty)

main :: IO ()
main = runInputT defaultSettings loop
  where
    loop = do
      minput <- getInputLine "TArith> "
      case minput of
        Nothing -> outputStrLn "Goodbye."
        Just input -> liftIO (process input) >> loop
