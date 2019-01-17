module FinanzasTest (finanzasTest) where

import Text.Printf
import Data.Servers.Finanzas
import qualified Data.Server as Sv
import qualified Data.Quote as Quote
import Control.Exception.Base

alist :: [(Bool -> String -> String, Bool)] -> String
alist ls = foldl (\acc (f, v) ->  (f v "") ++ acc) "" ls

finanzasTest :: IO ()
finanzasTest = do
  qs' <- Sv.read Finanzas "amadeus"
  case qs' of
    Left msg -> putStrLn $ "Fail" ++ msg
    Right qs -> do
      putStrLn "Test Finanzas:"
--      mapM_ (putStrLn . Quote.toStr) qs
      putStrLn $ alist [
          (assert, 254 == length qs)
        ] ++
        "    Finished"
