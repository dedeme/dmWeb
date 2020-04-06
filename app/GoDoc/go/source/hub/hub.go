// Copyright 01-Apr-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package hub

import (
	"github.com/dedeme/GoDoc/source/changepass"
	"github.com/dedeme/GoDoc/source/mainsrc"
	"github.com/dedeme/GoDoc/source/settings"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/json"
  "fmt"
)

func Process(ck string, rq map[string]json.T) string {
	source := cgi.RqString(rq, "source")

	switch source {
	case "Main":
		return mainsrc.Process(ck, rq)
	case "ChangePass":
		return changepass.Process(ck, rq)
	case "Settings":
		return settings.Process(ck, rq)
	default:
		panic(fmt.Sprintf("Value '%v' for 'source' is not valid", source))
	}
}
