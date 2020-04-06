// Copyright 01-Apr-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package changepass

import (
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/json"
)

func Process(ck string, mrq map[string]json.T) string {
	user := cgi.RqString(mrq, "user")
	old := cgi.RqString(mrq, "old")
	new := cgi.RqString(mrq, "new")
  return cgi.ChangePass(ck, user, old, new)
}
