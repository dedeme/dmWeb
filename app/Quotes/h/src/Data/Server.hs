-- Copyright 30-Nov-2018 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>

-- | Server class

module Data.Server (
  Server (..)
  ) where

import qualified Data.Quote
import Data.Quote (Quote)

class Server a where
  -- | @'name' server@ - Returns the name of /server/.
  name :: a -> String
  -- | @'uri' server code@ - Returns the uri of company 'code' in /server/
  --
  -- /code/ is the server code of a company.
  uri :: a -> String -> String
  -- | @'read' server code@ - Read last quotes of a company of /server/
  --
  -- /code/ is the server code of a company.
  read :: a -> String -> IO (Either String [Quote])

