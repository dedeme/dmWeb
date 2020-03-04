-- Copyright 14-Feb-2020 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

{-# LANGUAGE NamedFieldPuns #-}

--- Module page.

module Module (Init(..), T(wg), new) where

import Dm.Dom (Element, Property(..), q, q', qSel)
import qualified Dm.Client as Client
import qualified Dm.Js as Js
--import qualified Dm.Str as Str
--import I18n

--- Init

data Init = Init
  { client :: Client.T
  , pathName :: String
  , modulePath :: String
  , filePath :: String
  }

--- T
data T = T
          { ini :: Init
          , code :: String

          , wg :: Element
          }

-- CONSTRUCTOR

--- new ini fn
--- Constructor
new :: Init -> (Maybe T -> Fay ()) -> Fay ()
new ini fn = do
  Client.send (client ini)
    [ ("source", Js.ws "Code")
    , ("path", Js.ws $ filePath ini)
    ]
    ( \r -> do
        wg <- q "div" [] []
        let code = Client.rrp r "code" (Js.rMaybe Js.rs)
        case code of
          Nothing -> fn Nothing
          Just c -> do
            let this = T { ini, code = c, wg }
            view this
            fn $ Just this
    )

-- VIEW

view :: T -> Fay ()
view this = do
  _ <- qSel "title" [ Text (modulePath (ini this)) ] []
  _ <- q' (wg this) []
          [ q "p" [ Html "module" ] []]
  return ()
