// Copyright 12-Oct-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Main javascript page
package main

import (
	"fmt"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/json"
)

func mainProcess(ck string, mrq map[string]json.T) string {
	rq := cgi.RqString(mrq, "rq")
	switch rq {
	case "read":
		return cgi.Rp(ck, map[string]json.T{
			"data": readSettings(), // db.go file
		})
	case "close":
		sessionId := cgi.RqString(mrq, "sessionId")
		return cgi.DelSession(ck, sessionId)
	default:
		panic(fmt.Sprintf("Value of source ('%v') is not valid", rq))
	}
}
