// Copyright 18-06-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Paths javascript page
package main

import (
	"fmt"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/json"
)

func pathsProcess(ck string, mrq map[string]json.T) string {
	rq := cgi.RqString(mrq, "rq")
	switch rq {
	case "update":
		data := mrq["data"]
		writeSettings(data) // db.go file
		return cgi.Rp(ck, map[string]json.T{
			"data": readSettings(), // db.go file
		})
	default:
		panic(fmt.Sprintf("Value of source ('%v') is not valid", rq))
	}
}
