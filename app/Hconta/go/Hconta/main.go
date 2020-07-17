// Copyright 18-Jun-2020 ºDeme
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
		timeStamp, year, years, lang, data := readData() // db.go
		return cgi.Rp(ck, map[string]json.T{
			"timeStamp": timeStamp,
			"year":      year,
			"years":     years,
      "lang":      lang,
			"data":      data,
		})
	case "write":
		timeStamp := writeData(
      mrq["timeStamp"], mrq["year"], mrq["lang"], mrq["data"],
    ) // db.go
		return cgi.Rp(ck, map[string]json.T{
			"timeStamp": timeStamp, // If fail timeStamp == json.Ws("")
		})
	case "year":
		timeStamp := writeYear(mrq["timeStamp"], mrq["year"]) // db.go
		return cgi.Rp(ck, map[string]json.T{
			"timeStamp": timeStamp, // If fail timeStamp == json.Ws("")
		})
	case "close":
		sessionId := cgi.RqString(mrq, "sessionId")
		return cgi.DelSession(ck, sessionId)
	default:
		panic(fmt.Sprintf("Value of source ('%v') is not valid", rq))
	}
}
