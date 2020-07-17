// Copyright 06-Jul-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Sources page.
package main

import (
	"fmt"
	"github.com/dedeme/News/db/eauthors"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/json"
)

func authorsProcess(ck string, mrq map[string]json.T) string {
	rq := cgi.RqString(mrq, "rq")
	switch rq {
	case "read":
		var ss []json.T
		for i, e := range eauthors.Read() {
			if i == 100 {
				break
			}
			ss = append(ss, e.ToJs())
		}
		return cgi.Rp(ck, map[string]json.T{"wgs": json.Wa(ss)})
	default:
		panic(fmt.Sprintf("Value of rq ('%v') is not valid", rq))
	}
}
