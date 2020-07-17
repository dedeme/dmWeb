// Copyright 06-Jul-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Main go file
package main

import (
	"fmt"
	"github.com/dedeme/News/data/cts"
	"github.com/dedeme/News/db/conf"
	"github.com/dedeme/News/db/eauthors"
	"github.com/dedeme/News/db/esources"
	"github.com/dedeme/News/db/evalWeights"
	"github.com/dedeme/News/db/ewords"
	"github.com/dedeme/News/db/newss"
	"github.com/dedeme/News/local"
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
		return mainProcess(ck, mrq) // main.go
	case "News":
		return newsProcess(ck, mrq) // newsPage.go
	case "Weights":
		return weightsProcess(ck, mrq) // weightsPage.go
	case "Sources":
		return sourcesProcess(ck, mrq) // sourcesPage.go
	case "Authors":
		return authorsProcess(ck, mrq) // authorsPage.go
	case "Words":
		return wordsProcess(ck, mrq) // wordsPage.go
	case "ChangePass":
		return changePassProcess(ck, mrq) // changePass.go
	default:
		panic(fmt.Sprintf("Value of source ('%v') is not valid", source))
	}
}

func main() {
	if len(os.Args) != 2 {
		fmt.Println("Hconta need one and only one argument.")
		return
	}
	rq := os.Args[1]

	// Local ---------------------------------------------------------------------
	if rq == "run" || rq == "test" {
		local.Process(rq)
		return
	}

	// Web -----------------------------------------------------------------------
	cgi.Initialize(cts.HOME, 900) // Web application.

	ix := strings.IndexByte(rq, ':')
	//............................................................... CONNECTION
	if ix == -1 {
		fmt.Print(cgi.Connect(rq))
		return
	}

	//........................................................... AUTHENTICATION
	if ix == 0 {
		key := cryp.Key(cts.APP, cgi.Klen)
		data := cryp.Decryp(key, rq[1:])
		ps := strings.Split(data, ":")
		fmt.Print(cgi.Authentication(key, ps[0], ps[1], ps[2] == "1"))

		conf.Init()
		evalWeights.Init()
		esources.Init()
		eauthors.Init()
		ewords.Init()
		newss.WebInit()

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
