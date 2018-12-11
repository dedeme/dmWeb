module InvertiaTest (invertiaTest) where

import Text.Printf
import Data.Servers.Invertia
import qualified Data.Server as Sv
import qualified Data.Quote as Quote
import Control.Exception.Base

alist :: [(Bool -> String -> String, Bool)] -> String
alist ls = foldl (\acc (f, v) ->  (f v "") ++ acc) "" ls

invertiaTest :: IO ()
invertiaTest = do
  qs' <- Sv.read Invertia "RV011ACERINO"
  case qs' of
    Left msg -> putStrLn $ "Fail" ++ msg
    Right qs -> do
      putStrLn "Test Invertia:"
--      mapM_ (putStrLn . Quote.toStr) qs
      putStrLn $ alist [
          (assert, 34 == length qs)
        ] ++
        "    Finished"
