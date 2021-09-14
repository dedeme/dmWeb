// Copyright 14-Sep-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Main go file
package main

import (
	"fmt"
	"github.com/dedeme/Bet1x2/data/cts"
	"github.com/dedeme/Bet1x2/pgs/changePass"
	"github.com/dedeme/Bet1x2/pgs/mainPg"
	"github.com/dedeme/Bet1x2/db"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/cryp"
	"github.com/dedeme/golib/json"
	"os"
	"strings"
)

func sourceProcess(ck string, mrq map[string]json.T) string {
	source := cgi.RqString(mrq, "source")
	switch source {
	case "Main":
		return mainPg.Process(ck, mrq)
//	case "Year":
//		return year.Process(ck, mrq)
	case "ChangePass":
		return changePass.Process(ck, mrq)
	default:
		panic(fmt.Sprintf("Value of source ('%v') is not valid", source))
	}
}

func main() {
	if len(os.Args) != 2 {
		fmt.Println("Bet1x2 need one and only one argument.")
		return
	}
	rq := os.Args[1]

	cgi.Initialize(cts.Home, 900)
	db.Initialize()

	ix := strings.IndexByte(rq, ':')
	//............................................................... CONNECTION
	if ix == -1 {
		fmt.Print(cgi.Connect(rq))
		return
	}

	//........................................................... AUTHENTICATION
	if ix == 0 {
		key := cryp.Key(cts.App, cgi.Klen)
		data := cryp.Decryp(key, rq[1:])
		ps := strings.Split(data, ":")
		fmt.Print(cgi.Authentication(key, ps[0], ps[1], ps[2] == "1"))
		return
	}

	//............................................................... NORMAL DATA
	sessionId := rq[:ix]
	conKey := ""
	rest := rq[ix+1:]
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
	fmt.Print(sourceProcess(comKey, data))
}
