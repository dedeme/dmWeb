// Copyright 30-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Main file.
package main

import (
	"fmt"
	"github.com/dedeme/GoDoc/data/cts"
	"github.com/dedeme/GoDoc/db"
	"github.com/dedeme/GoDoc/source/hub"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/cryp"
	"github.com/dedeme/golib/json"
	"github.com/dedeme/golib/log"
	"os"
	"strings"
)

func main() {
	defer func() {
		if err := recover(); err != nil {
			log.Print(err)
		}
	}()

	rq := os.Args[1]
	cgi.Initialize("/dm/wwwcgi/dmcgi/"+cts.AppName, 900)
	db.Initialize()

	ix := strings.IndexByte(rq, ':')
	//............................................................... CONNECTION
	if ix == -1 {
		fmt.Print(cgi.Connect(rq))
		return
	}

	//........................................................... AUTHENTICATION
	if ix == 0 {
		key := cryp.Key(cts.AppName, cgi.Klen)
		data := cryp.Decryp(key, rq[1:])
    ps := strings.Split(data, ":")
		fmt.Print(cgi.Authentication(key, ps[0], ps[1], ps[2] == "1"))
		return
	}

	//............................................................... NORMAL DATA
	sessionId := rq[:ix]
  conKey := ""
  rest := rq[ix + 1:]
  ix = strings.IndexByte(rest, ':')
  if ix != -1 {
    conKey = rest[:ix]
    rest = rest[ix+1:]
  }
	comKey, ok := cgi.GetComKey(sessionId, conKey)
	if !ok {
		fmt.Print(cgi.RpExpired())
		return
	}
	js := cryp.Decryp(comKey, rest)
	data := json.FromString(js).Ro()
	fmt.Print(hub.Process(comKey, data))
}
