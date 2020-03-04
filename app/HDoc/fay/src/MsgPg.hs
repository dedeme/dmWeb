-- Copyright 19-Feb-2020 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

{-# LANGUAGE NamedFieldPuns #-}

--- Message page.

module MsgPg (Init(..), T(wg), new) where

import Dm.Dom (Element, Property(..), q, q')
import I18n

--- Init

data Init = Init
  { title :: String      -- Page title
  , msg :: String        -- Message in html
  , reload :: Bool       -- If there will be a message for reloading.
  }

--- T
data T = T
          { ini :: Init
          , wg :: Element
          }

-- CONSTRUCTOR

--- new ini
--- Constructor
new :: Init -> Fay T
new ini = do
  wg <- q "div" [] []

  let this = T
              { ini
              , wg
              }

  view this
  return this

-- VIEW

view :: T -> Fay ()
view this = do
  let link = if reload (ini this)
                then i18nFormat (__ "Click %0 to continue")
                      ["<a href=''>" ++ (__ "here") ++ "</a>"]
                else ""
  _ <- q' (wg this) [RemoveAll]
        [ q "div"
          [ Class "head", Text $ title (ini this)] []
        , q "p" [ Style "width:30px" ] []
        , q "table" [ Att "align" "center" ]
            [ q "tr" []
                [ q "td" [ Class "frame", Html $ msg (ini this) ] [] ]
            ]
        , q "p" [ Style "text-align:center", Html link ] []
        ]
  return ()

