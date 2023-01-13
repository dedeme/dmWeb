// Copyright 18-Jun-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Main javascript page
package main

import (
	"github.com/dedeme/ktlib/cgi"
	"github.com/dedeme/ktlib/file"
	"github.com/dedeme/ktlib/js"
	"github.com/dedeme/ktlib/path"
	"github.com/dedeme/ktlib/str"
)

func mainProcess(ck string, mrq map[string]string) string {
	rq := js.Rs(mrq["rq"])
	switch rq {
	case "read":
		dir := path.Cat(HOME, "data")
		if !file.Exists(dir) {
			file.Mkdir(dir)
		}
		f := path.Cat(dir, "all.tb")
		if !file.Exists(f) {
			return cgi.Rp(ck, map[string]string{
				"data": js.Wn(),
			})
		}
		return cgi.Rp(ck, map[string]string{
			"data": file.Read(f),
		})
	case "write":
		f := path.Cat(HOME, "data", "all.tb")
		file.Write(f, mrq["data"])
		return cgi.RpEmpty(ck)
	case "close":
		sessionId := js.Rs(mrq["sessionId"])
		return cgi.DelSession(ck, sessionId)
	default:
		panic(str.Fmt("Value of source ('%v') is not valid", rq))
	}
}
