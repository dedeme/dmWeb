// Copyright 05-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Main hub to process client request.
package mainHub

import (
	"fmt"
	"github.com/dedeme/MrBackup/data/cts"
	"github.com/dedeme/MrBackup/data/logRow"
	"github.com/dedeme/MrBackup/db/conf"
	"github.com/dedeme/MrBackup/server/dirRow"
	"github.com/dedeme/MrBackup/server/pgs/directories"
	"github.com/dedeme/MrBackup/server/pgs/settings"
	"github.com/dedeme/MrBackup/server/pgs/summary"
	"github.com/dedeme/ktlib/cgi"
	"github.com/dedeme/ktlib/cryp"
	"github.com/dedeme/ktlib/js"
	"strings"
)

func subHub(ck string, mrq map[string]string) string {
	rq := js.Rs(mrq["rq"])
	switch rq {
	case "conf":
		return cgi.Rp(ck, map[string]string{
			"lang": js.Ws(conf.Lang()),
			"menu": js.Ws(conf.Menu()),
		})
	case "menuOption":
		option := js.Rs(mrq["option"])
		conf.SetMenu(option)
		return cgi.RpEmpty(ck)
	case "bye":
		sessionId := js.Rs(mrq["sessionId"])
		return cgi.DelSession(ck, sessionId)
	default:
		panic(fmt.Sprintf("Value of rq ('%v') is not valid", rq))
	}
}

func hub(ck string, mrq map[string]string) string {
	page := js.Rs(mrq["page"])
	switch page {
	case "main":
		return subHub(ck, mrq)
	case "summary":
		return summary.Process(ck, mrq)
	case "directories":
		return directories.Process(ck, mrq)
	case "dirRow":
		return dirRow.Process(ck, mrq)
	case "settings":
		return settings.Process(ck, mrq)
	default:
		panic(fmt.Sprintf("Value of page ('%v') is not valid", page))
	}
}

func Process(rq string) (rp string) {
	defer func() {
		if r := recover(); r != nil {
			rp = logRow.Format(fmt.Sprint(r))
		}
	}()

	ix := strings.IndexByte(rq, ':')
	//............................................................... CONNECTION
	if ix == -1 {
		return cgi.Connect(rq)
	}

	//........................................................... AUTHENTICATION
	if ix == 0 {
		key := cryp.Key(cts.AppName, cgi.Klen)
		data := cryp.Decode(key, rq[1:])
		ps := strings.Split(data, ":")
		return cgi.Authentication(key, ps[0], ps[1], ps[2] == "1")
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
		return cgi.RpExpired()
	}

	j := cryp.Decode(comKey, rest)
	data := js.Ro(j)
	return hub(comKey, data)
}
