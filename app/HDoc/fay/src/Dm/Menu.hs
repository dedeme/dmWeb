-- Copyright 27-Jan-2020 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

{-# LANGUAGE NamedFieldPuns #-}

--- Menu widget.

module Dm.Menu
  ( Init(..)
  , T
  , Entry(..)
  , new
  , getSelected
  , getLeft
  , getRight
  , wg
  , separator
  , separator2
  , txOption
  , imgOption
  , txLink
  , imgLink
  , close
  ) where

import Dm.Dom (Element, Property(..), q, q')
import qualified Dm.Ui as Ui

--- Init
---   { selected :: String    -- Identifier of selected Entry
---   , left :: [Entry]       -- Entries of left part of menu
---   , right :: [Entry]      -- Entries of Right part of menu
---   , withSeparator :: Bool -- Put a separator at right of left part.
---   }
data Init = Init
  { selected :: String    -- Identifier of selected Entry
  , left :: [Entry]       -- Entries of left part of menu
  , right :: [Entry]      -- Entries of Right part of menu
  , withSeparator :: Bool -- Put a separator at right of left part.
  }

--- T
--- Menu type
data T = T
  { ini :: Init

  , wg :: Element
  }

--- Entry
---   = WgEntry Element
---   | MenuEntry String Element
data Entry = WgEntry Element
           | MenuEntry String Element

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

--- getSelected menu
--- Return item selected identifier.
getSelected :: T -> String
getSelected menu = selected $ ini menu

--- getLeft menu
--- Returns left entries.
getLeft :: T -> [Entry]
getLeft menu = left $ ini menu

--- getRight menu
--- Returns right entries.
getRight :: T -> [Entry]
getRight menu = right $ ini menu

-- VIEW

view :: T -> Fay ()
view this = do
  _ <- q' (wg this) [ RemoveAll ]
        [ q "table" [ Style "border-collapse:collapse; width:100%" ]
            [ q "tr" []
                [ q "td"
                    [ Style $ "padding-right:4px;vertical-align:top;" ++
                              ( if withSeparator (ini this)
                                  then "border-right: 1px solid #000000;"
                                  else ""
                              )
                    ] [
                      q "div" [ Style "line-height:22px;" ]
                        $ map fmt $ left (ini this)
                    ]
                , q "td"
                    [ Style "padding-left:4px;vertical-align:top;\
                            \text-align:right;white-space:nowrap"
                    ] [ q "div" [] $ map fmt $ right (ini this) ]
                ]
            ]
        , q "hr" [] []
        ]
  return ()
  where
    fmt :: Entry -> Fay Element
    fmt (WgEntry e) = return e
    fmt (MenuEntry mId e)
      | mId == selected (ini this) =
        q' e
          [ AddStyle "background-color" "rgb(250, 250, 250)"
          , AddStyle "border" "1px solid rgb(110,130,150)"
          , AddStyle "border-radius" "4px"
          , AddStyle "padding" "4px"
          , AddStyle "vertical-align" "top"
--          Style "cursor:pointer;\
--                  \background-color: rgb(250, 250, 250);\
--                  \border: 1px solid rgb(110,130,150);\
--                  \font-family: sans; font-size: 14px;\
--                  \padding: 4px;border-radius: 4px;"
          ] []
      | True = return e

-- PUBLIC FUNCTIONS

--- separator
--- Makes a new separator " · "
separator :: Fay Entry
separator = q "span" [ Text " · " ] [] >>= (return . WgEntry)

--- separator2
--- Makes a new separator " | "
separator2 :: Fay Entry
separator2 = q "span" [ Text " | " ] [] >>= (return . WgEntry)

--- txOption id html fn
--- Creates an Entry with a text.
---   id  : Identifier.
---   html: Entry text in 'html' format.
---   fn  : Callback.
txOption :: String -> String -> Fay () -> Fay Entry
txOption mId html fn = Ui.tLink html fn >>= (return . MenuEntry mId)

--- imgOption id name fn
--- Creates an Entry with an element.
---   id  : Identifier.
---   e   : Element. It will usualy be an image.
---   fn  : Callback.
imgOption :: String -> Element -> Fay () -> Fay Entry
imgOption mId e fn = Ui.eLink e fn >>= (return . MenuEntry mId)

--- txLink group id html
--- Creates a link Entry with a text.
--- The link is formed 'Main.urlBase + "?" ++ group + "&" ++ id' or
--- 'Main.urlBase + "?" ++ id' if module is not defined.
---   group: group
---   id   : Identifier.
---   html : Entry text in 'html' format.
txLink :: Maybe String -> String -> String -> Fay Entry
txLink mGroup mId html =
  let path = case mGroup of
               Just g -> "?" ++ g ++ "&" ++ mId
               Nothing -> "?" ++ mId
  in  q "a" [ Att "href" path, Html html ] [] >>= (return . MenuEntry mId)

--- imgLink mGroup mId name
--- Creates a link Entry with an element.
--- The link is formed 'Main.urlBase + "?" ++ group + "&" ++ id' or
--- 'Main.urlBase + "?" ++ id' if module is not defined.
---   group: group
---   id   : Identifier.
---   e   : Element. It will usualy be an image.
imgLink :: Maybe String -> String -> Element -> Fay Entry
imgLink mGroup mId e =
  let path = case mGroup of
               Just g -> "?" ++ g ++ "&" ++ mId
               Nothing -> "?" ++ mId
  in  q "a" [ Att "href" path ] [ return e ] >>= (return . MenuEntry mId)

close :: Fay () -> Fay Entry
close fbye = do
  img <- Ui.img "cross"
  img' <- q' img [Style "vertical-align:bottom"] []
  link <- Ui.eLink img' fbye
  return $ WgEntry link
