// Copyright 18-Jun-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Main javascript page
package main

import (
	"fmt"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/json"
	"github.com/dedeme/golib/file"
  "path"
)

func mainProcess(ck string, mrq map[string]json.T) string {
	rq := cgi.RqString(mrq, "rq")
	switch rq {
	case "read":
    dir := path.Join(HOME, "data")
    if !file.Exists(dir) {
      file.Mkdir(dir)
    }
    f := path.Join(dir, "all.tb")
    if !file.Exists(f) {
      return cgi.Rp(ck, map[string]json.T{
        "data": json.Wn(),
      })
    }
    return cgi.Rp(ck, map[string]json.T {
      "data": json.FromString(file.ReadAll(f)),
    })
	case "write":
    f := path.Join(HOME, "data", "all.tb")
    file.WriteAll(f, mrq["data"].String())
    return cgi.RpEmpty(ck)
	case "close":
		sessionId := cgi.RqString(mrq, "sessionId")
		return cgi.DelSession(ck, sessionId)
	default:
		panic(fmt.Sprintf("Value of source ('%v') is not valid", rq))
	}
}
