// Copyright 05-Apr-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// ChangePass source hub.
package chpass

import (
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/json"
)

func Process(ac chan func(), ck string, mrq map[string]json.T) string {
  return cgi.ChangePass(
    ck,
    cgi.RqString(mrq, "user"),
    cgi.RqString(mrq, "old"),
    cgi.RqString(mrq, "new"),
  )
}
