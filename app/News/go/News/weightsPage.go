// Copyright 06-Jul-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Weights page.
package main

import (
	"fmt"
	"github.com/dedeme/News/db/evalWeights"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/json"
)

func weightsProcess(ck string, mrq map[string]json.T) string {
	rq := cgi.RqString(mrq, "rq")
	switch rq {
	case "read":
		we := evalWeights.Read()
		return cgi.Rp(ck, map[string]json.T{
			"source": json.Wd(we.Source),
			"author": json.Wd(we.Author),
			"words":  json.Wd(we.Words),
		})
	default:
		panic(fmt.Sprintf("Value of rq ('%v') is not valid", rq))
	}
}
