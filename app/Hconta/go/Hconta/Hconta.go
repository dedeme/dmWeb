// Copyright 18-Jun-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Main go file
package main

import (
	"github.com/dedeme/Hconta/stocks"
	"github.com/dedeme/ktlib/cgi"
	"github.com/dedeme/ktlib/cryp"
	"github.com/dedeme/ktlib/js"
	"github.com/dedeme/ktlib/str"
	"github.com/dedeme/ktlib/sys"
)

func sourceProcess(ck string, mrq map[string]string) string {
	source := js.Rs(mrq["source"])
	switch source {
	case "Main":
		return mainProcess(ck, mrq) // main.go
	case "ChangePass":
		return changePassProcess(ck, mrq) // changePass.go
	case "Stocks":
		return stocks.Process(ck, mrq)
	default:
		panic(str.Fmt("Value of source ('%v') is not valid", source))
	}
}

func main() {
	args := sys.Args()
	if len(args) != 2 {
		sys.Println("Hconta need one and only one argument.")
		return
	}
	rq := args[1]

	cgi.Initialize(HOME, 900)

	ix := str.IndexByte(rq, ':')
	//............................................................... CONNECTION
	if ix == -1 {
		sys.Print(cgi.Connect(rq))
		return
	}

	//........................................................... AUTHENTICATION
	if ix == 0 {
		key := cryp.Key(APP, cgi.Klen)
		data := cryp.Decode(key, rq[1:])
		ps := str.Split(data, ":")
		sys.Print(cgi.Authentication(key, ps[0], ps[1], ps[2] == "1"))
		return
	}

	//............................................................... NORMAL DATA
	sessionId := rq[:ix]
	conKey := ""
	rest := rq[ix+1:]
	ix = str.IndexByte(rest, ':')
	if ix != -1 {
		conKey = rest[:ix]
		rest = rest[ix+1:]
	}
	comKey, ok := cgi.GetComKey(sessionId, conKey)
	if !ok {
		sys.Print(cgi.RpExpired())
		return
	}

	j := cryp.Decode(comKey, rest)
	data := js.Ro(j)
	sys.Print(sourceProcess(comKey, data))
}
