module Main where

import Parser
import Eval

import Control.Monad.Trans
import System.Console.Haskeline

process :: String -> IO ()
process line = do
  let res = parseExpr line
  case res of
    Left err -> print err
    Right ex -> case eval ex of
                  Nothing -> putStrLn "Cannot evaluate"
                  Just result -> print result

main :: IO ()
main = runInputT defaultSettings loop
  where
    loop = do
      minput <- getInputLine "Repl> "
      case minput of
        Nothing -> outputStrLn "Goodbye."
        Just input -> liftIO (process input) >> loop
