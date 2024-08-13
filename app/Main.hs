module Main (main) where

import Data.Char (chr, isAsciiLower, ord)
import Data.List (isSuffixOf)
import qualified Data.Map as Map
import ExprParser (eval)
import Numeric (showFFloat)
import System.Console.Haskeline (InputT, defaultSettings, getInputLine, outputStrLn, runInputT)

main :: IO ()
main = runInputT defaultSettings $ do
  outputStrLn $
    unlines
      [ "shellculator 0.1.0.0",
        "==================================",
        "For usage information enter 'help'"
      ]
  loop 'a' Map.empty

-- Loops until the user quits
loop :: Char -> Map.Map Char Double -> InputT IO ()
loop key history = do
  input <- getInputLine ">> "
  case input of
    Nothing -> return ()
    Just cmd | cmd `elem` ["q", "quit", "Q", "exit"] -> return ()
    Just "help" -> outputStrLn usage >> loop key history
    Just expr -> do
      let expr' = replaceLetters history expr
      let (result, history', key') =
            maybe ("Invalid expression", history, key) (updateState history key) $ eval expr'
      outputStrLn result
      loop key' history'

-- Replaces key to the value that the key represents
replaceLetters :: Map.Map Char Double -> String -> String
replaceLetters history = concatMap replace
  where
    replace c
      | validChar c = maybe [c] showNumber $ Map.lookup c history
      | otherwise = [c]
    showNumber d = ' ' : showFFloat Nothing d ""

-- Updates history, key and the result of evaluating the input
updateState :: Map.Map Char Double -> Char -> Double -> (String, Map.Map Char Double, Char)
updateState history key expr = (resultAndKey, history', key')
  where
    resultAndKey = result' ++ replicate len ' ' ++ ['[', key, ']']
    result'
      | isNaN expr = "NaN"
      | ".0" `isSuffixOf` result = takeWhile (/= '.') result
      | otherwise = result
    result = show expr
    len = distanceToKey - length result'
    history' = Map.insert key expr history
    key' = until validChar incr $ incr key
    incr x = if isAsciiLower x then chr $ ord x + 1 else 'a'

validChar :: Char -> Bool
validChar c = isAsciiLower c && notElem c takenChars
  where
    takenChars = "e" ++ "pi" ++ "sqrt"

distanceToKey :: Int
distanceToKey = 30

usage :: String
usage =
  unlines
    [ "==================================",
      "",
      "Usage:",
      "  Enter any arithmetic expression using numbers, +, -, *, /, (), sqrt(), e and pi.",
      "  Example: ",
      "  >> 3 * (2 + 5)",
      "  21                          [a]",
      "",
      "  You can use previously calculated results by referring to their letters.",
      "  For example: ",
      "  >> a * 2",
      "  42                          [b]",
      "",
      "Commands:",
      "  help  - Show this usage information",
      "  quit  - Quit the calculator",
      "  q     - Quit the calculator",
      "",
      "Note:",
      "  - The calculator automatically assigns results to letters starting from 'a'.",
      "  - It avoids using 'e', 'p', 'i' and so on, as they are reserved."
    ]