module YahooTest (yahooTest) where

import Text.Printf
import Data.Servers.Yahoo
import qualified Data.Server as Sv
import qualified Data.Quote as Quote
import Control.Exception.Base

alist :: [(Bool -> String -> String, Bool)] -> String
alist ls = foldl (\acc (f, v) ->  (f v "") ++ acc) "" ls

yahooTest :: IO ()
yahooTest = do
  qs' <- Sv.read Yahoo "BBVA.MC"
  case qs' of
    Left msg -> putStrLn $ "Fail" ++ msg
    Right qs -> do
      putStrLn "Test Yahoo:"
--      mapM_ (putStrLn . Quote.toStr) qs
      putStrLn $ alist [
          (assert, 79 == length qs)
        ] ++
        "    Finished"
