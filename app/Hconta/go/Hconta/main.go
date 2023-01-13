// Copyright 18-Jun-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Main javascript page
package main

import (
	"github.com/dedeme/ktlib/cgi"
	"github.com/dedeme/ktlib/js"
	"github.com/dedeme/ktlib/str"
)

func mainProcess(ck string, mrq map[string]string) string {
	rq := js.Rs(mrq["rq"])
	switch rq {
	case "read":
		timeStamp, year, years, lang, data := readData() // db.go
		return cgi.Rp(ck, map[string]string{
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
		return cgi.Rp(ck, map[string]string{
			"timeStamp": timeStamp, // If fail timeStamp == json.Ws("")
		})
	case "year":
		timeStamp := writeYear(mrq["timeStamp"], mrq["year"]) // db.go
		return cgi.Rp(ck, map[string]string{
			"timeStamp": timeStamp, // If fail timeStamp == json.Ws("")
		})
	case "close":
		sessionId := js.Rs(mrq["sessionId"])
		return cgi.DelSession(ck, sessionId)
	default:
		panic(str.Fmt("Value of source ('%v') is not valid", rq))
	}
}
