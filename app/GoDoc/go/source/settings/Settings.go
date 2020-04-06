// Copyright 01-Apr-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package settings

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
	case "setLang":
    lang, ok := mrq["lang"]
    if !ok {
      panic("Field 'lang' is missing")
    }
    conf.SetLang(lang)
    return cgi.RpEmpty(ck)
  case "updateLibs":
    libType := plibs.Standard
    if cgi.RqBool(mrq, "isPersonal") {
      libType = plibs.Personal
    }
    pathsjs, ok  := mrq["paths"]
    if !ok {
      panic("Field 'libs' is missing")
    }
    var libs []string
    for _, l := range pathsjs.Ra() {
      libs = append(libs, l.Rs())
    }
    plibs.UpdateAll(libType, libs)
    return cgi.RpEmpty(ck)
	default:
		panic(fmt.Sprintf("Value '%v' for 'rq' is not valid", rq))
	}
}
