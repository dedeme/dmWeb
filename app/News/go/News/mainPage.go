// Copyright 06-Jul-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Main javascript page
package main

import (
	"fmt"
	"github.com/dedeme/News/db/conf"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/json"
)

func mainProcess(ck string, mrq map[string]json.T) string {
	rq := cgi.RqString(mrq, "rq")
	switch rq {
	case "read":
		conf := conf.Read()
		return cgi.Rp(ck, map[string]json.T{
			"conf": conf,
		})
	case "lang":
		lang := cgi.RqString(mrq, "lang")
		conf.SetLang(lang) // db.go
		return cgi.RpEmpty(ck)
	case "close":
		sessionId := cgi.RqString(mrq, "sessionId")
		return cgi.DelSession(ck, sessionId)
	default:
		panic(fmt.Sprintf("Value of rq ('%v') is not valid", rq))
	}
}
