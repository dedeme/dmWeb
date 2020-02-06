-- Copyright 22-Jan-2020 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

module I18n
  ( trs
  , format
  ) where

import FFI

en :: [(String, String)]
en =
  [
  ]

es :: [(String, String)]
es =
  [ ("Login", "Identificación")
  ]

get :: [(String, String)] -> String -> String
get ls s = case lookup s ls of Just v -> v; _ -> s

trs :: String -> (String -> String)
trs "es" = get es
trs _ = get en

format :: String -> [String] -> String
format tpl ls = let (r, _) = foldl replaceN (tpl, 0::Int) ls
                in  replace r "%%" "%"
  where
    replace :: String -> String -> String -> String
    replace = ffi "%1.replace(new RegExp(%2, 'g'), %3)"
    replaceN (t, n) s = (replace t ("%" ++ (show n)) s, n + 1)
