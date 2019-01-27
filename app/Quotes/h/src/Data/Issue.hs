-- Copyright 06-Dic-2018 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

-- | Issue data

module Data.Issue (
  T(..),
  Issue(..),
  toJs,
  fromJs,
  check,
  checkQuotes,
  annotate,
  search
  ) where

import Data.List
import qualified Dm.Js as Js
import Dm.Js (JSValue)
import qualified Data.NicksDb as NicksDb
import Data.NicksDb (NicksDb(..))
import qualified Data.ServersDb as ServersDb
import qualified Data.Quote as Q
import Data.Quote (Quote(..))
import Data.Nick (Nick(..))

-- | Issue type
data T =  None |      -- There is no issue
          Exists |    -- Nick_id does not exists
          Server |    -- Code of server is missing
          Empty |     -- Not data at all
          Missing |   -- Missing quote
          Extra |     -- Extra quote
          BeforeNow | -- Current quote varies +- 20%
          Max |       -- Open or Close > Max
          Min         -- Open or Close < Min
          deriving (Eq, Show, Enum)

-- | Issue data
data Issue = Issue {
  nickId :: String,
  tp :: T,
  cause :: String
} deriving Show

-- | @'toJs' iss@ - Parses /iss/ to JSON.
toJs :: Issue -> JSValue
toJs (Issue id tp cause) = Js.wList [
    Js.wString id, Js.wInt $ fromEnum tp, Js.wString cause
  ]

-- | @'fromJs' js@ - Retrieves an Issue JSONized.
fromJs :: JSValue -> Issue
fromJs js = let [id, tp, cause] = Js.rList js
                in  Issue (Js.rString id) (toEnum $ Js.rInt tp)
                          (Js.rString cause)

checkQuotes' :: String -> String -> [Quote] -> [Quote] -> IO Issue
checkQuotes' nickId nickName mqs qs@((Quote date open1 _ _ _ _ _):_) = do
  qs' <- if open1 < (-1.5) then regularize1 [] mqs else return qs
  let iss@(Issue _ tp _) = issues mqs qs'
  if tp == None then regularize2 qs' >> return iss
                else return iss
  where
    add [] qs' = qs'
    add (d:ds) qs' = add ds ((Quote d (-2) (-2) (-2) (-2) (-2) True):qs')
    regularize1 r ((Quote d _ _ _ _ _ _):mqs')
      | date < d = regularize1 (d:r) mqs'
      | otherwise = case r of
          [] -> return qs
          _ -> do
            let r' = add r qs
            NicksDb.writeQuotes nickName r'
            return r'
    issues _ [_] = Issue nickId None ""
    issues [] _= issues [] []
    issues ((Quote md mo mc mmx mmn mv me):mqs)
           ((Quote d o c mx mn v e):qs@((Quote _ o1 c1 mx1 mn1 v1 _):_)) =
      if md > d then Issue nickId Missing (md ++ ":")
      else if md < d then Issue nickId Extra (d ++ ":")
      else if e then issues mqs qs
      else if abs (o - o1) / o1 > 0.2 then msg BeforeNow "open"
      else if abs (c - c1) / c1 > 0.2 then msg BeforeNow "close"
      else if abs (mx - mx1) / mx1 > 0.2 then msg BeforeNow "max"
      else if abs (mn - mn1) / mn1 > 0.2 then msg BeforeNow "min"
      else if o > mx then msg Max "open"
      else if c > mx then msg Max "close"
      else if mn > mx then msg Max "min"
      else if o < mn then msg Min "open"
      else if c < mn then msg Min "close"
      else if mx < mn then msg Min "max"
      else issues mqs qs
        where
          msg tp tx = Issue nickId tp (d ++ (':':tx))
    regularize2 qs' =
      case map
            (\(Quote date _ _ _ _ _ _) ->
              Quote date (-1) (-1) (-1) (-1) (-1) True)
            (drop (length qs') mqs) of
        [] -> return ()
        qs'' -> NicksDb.writeQuotes nickName (qs' ++ qs'')

check' :: String -> String -> IO Issue
check' nickId nickName = do
  msg <- foldl' fserver (return "") $ ServersDb.listNames
  if msg == ""
    then do
      qs' <- rQuotes nickId
      case qs' of
        Nothing -> return $ Issue nickId Empty nickName ----------------- Empty
        Just qs -> do
          NicksDb _ modelId _ <- NicksDb.read
          mqs' <- rQuotes modelId
          case mqs' of
            Nothing -> do
              (Right modelName) <- NicksDb.nickName modelId
              return $ Issue nickId Empty modelName --------------------- Empty
            Just mqs -> checkQuotes' nickId nickName mqs qs

    else return $ Issue nickId Server msg ------------------------------ Server
  where
    fserver r sv = do
      msg <- r
      if msg == "" then do
                      nks <- ServersDb.nicks sv
                      case lookup nickId nks of
                        Nothing -> return sv
                        _ -> r
                   else r
    rQuotes nId = do
      qs' <- NicksDb.quotes nId
      case qs' of
        Left _ -> return Nothing
        Right qs -> if length qs > 0 then return $ Just qs else return Nothing

-- | @'check' nickId@ - Returns the main Issue of /nickId/
check :: String -> IO Issue
check nickId = do
  nickName' <- NicksDb.nickName nickId
  case nickName' of
    Left e -> return $ Issue nickId Exists e --------------------------- Exists
    Right nickName -> check' nickId nickName

-- | @'checkQuotes' nickId qs@ - Return issues of /nickId/ /qs/
checkQuotes :: String -> [Quote] -> IO Issue
checkQuotes nickId qs = do
  nickName' <- NicksDb.nickName nickId
  case nickName' of
    Left e -> return $ Issue nickId Exists e --------------------------- Exists
    Right nickName -> do
      NicksDb _ modelId _ <- NicksDb.read
      mqs' <- rQuotes modelId
      case mqs' of
        Nothing -> do
          (Right modelName) <- NicksDb.nickName modelId
          return $ Issue nickId Empty modelName ------------------------- Empty
        Just mqs -> checkQuotes' nickId nickName mqs qs
  where
    rQuotes nId = do
      qs' <- NicksDb.quotes nId
      case qs' of
        Left _ -> return Nothing
        Right qs -> if length qs > 0 then return $ Just qs else return Nothing

-- | @'annotate' qs issue@ - Returns /qs/ converted to String an annotated
--                           indicating the point of the /issue/.
annotate :: [Quote] -> Issue -> String
annotate qs (Issue _ _ cause) =
  let date = takeWhile (/= ':') cause
      (left, right) = span (\(Quote date' _ _ _ _ _ _) -> date' > date) qs
      lefts = map Q.toStr left
      rights = map Q.toStr right
  in  (unlines lefts) ++ "% ******************\n" ++ (unlines rights)

-- | @'search'@ - Returns the nex Issue or Nothing if it is not found.
search :: IO (Maybe Issue)
search = do
  NicksDb _ _ list <- NicksDb.read
  nextIssue list
  where
    nextIssue [] = return Nothing
    nextIssue ((Nick id _ _ _ _):ns) = do
      issue@(Issue _ tp _) <- check id
      if tp == None then nextIssue ns
                    else return $ Just issue

