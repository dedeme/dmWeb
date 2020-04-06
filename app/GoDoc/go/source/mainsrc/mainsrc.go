// Copyright 01-Apr-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package mainsrc

import (
	"github.com/dedeme/GoDoc/db/conf"
	"github.com/dedeme/GoDoc/db/plibs"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/json"
	"fmt"
)

func Process(ck string, mrq map[string]json.T) string {
	rq := cgi.RqString(mrq, "rq")

	switch rq {
	case "idata":
    rawLib := cgi.RqString(mrq, "lib")
    rawLib = conf.Lib(rawLib)

    if rawLib != "@" && rawLib != "/" && rawLib != "*/" {
      lib := rawLib
      libType := plibs.Personal
      if rawLib[0] == '*' {
        lib = rawLib[1:]
        libType = plibs.Standard
      }
      plibs.Update(libType, lib)
    }

		r := map[string]json.T{}
		r["lib"] = json.Ws(rawLib)
		r["lang"] = conf.Lang()
    r["plibs"] = plibs.Read(plibs.Personal)
    r["slibs"] = plibs.Read(plibs.Standard)
		return cgi.Rp(ck, r)
	case "bye":
		sessionId := cgi.RqString(mrq, "sessionId")
    return cgi.DelSession(ck, sessionId)
	default:
		panic(fmt.Sprintf("Value '%v' for 'rq' is not valid", rq))
	}
}
