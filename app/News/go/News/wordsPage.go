// Copyright 06-Jul-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Sources page.
package main

import (
	"fmt"
	"github.com/dedeme/News/db/ewords"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/json"
)

func wordsProcess(ck string, mrq map[string]json.T) string {
	rq := cgi.RqString(mrq, "rq")
	switch rq {
	case "letters":
		var jss []json.T
		for _, e := range ewords.ReadLetters() {
			jss = append(jss, json.Ws(e))
		}
		return cgi.Rp(ck, map[string]json.T{"letters": json.Wa(jss)})
	case "read":
		letter := cgi.RqString(mrq, "letter")
		var ss []json.T
		for i, e := range ewords.Read(letter) {
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
