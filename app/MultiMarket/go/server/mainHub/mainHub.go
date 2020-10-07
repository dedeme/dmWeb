// Copyright 15-Aug-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Main hub to process client request.
package mainHub

import (
	"fmt"
	"github.com/dedeme/MultiMarket/data/cts"
	"github.com/dedeme/MultiMarket/data/logRow"
	"github.com/dedeme/MultiMarket/db/conf"
	"github.com/dedeme/MultiMarket/server/pgs/acc"
	"github.com/dedeme/MultiMarket/server/pgs/daily"
	"github.com/dedeme/MultiMarket/server/pgs/fleas"
	"github.com/dedeme/MultiMarket/server/pgs/home"
	"github.com/dedeme/MultiMarket/server/pgs/ranking"
	"github.com/dedeme/MultiMarket/server/pgs/settings"
	"github.com/dedeme/MultiMarket/server/pgs/wgs"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/cryp"
	"github.com/dedeme/golib/json"
	"strings"
)

func subHub(ck string, mrq map[string]json.T) string {
	rq := cgi.RqString(mrq, "rq")
	switch rq {
	case "lang":
		return cgi.Rp(ck, map[string]json.T{"lang": json.Ws(conf.Lang())})
	default:
		panic(fmt.Sprintf("Value of rq ('%v') is not valid", rq))
	}
}

func hub(ck string, mrq map[string]json.T) string {
	module := cgi.RqString(mrq, "module")
	switch module {
	case "main":
		return subHub(ck, mrq)
	case "wgs":
		return wgs.Process(ck, mrq)
	case "home":
		return home.Process(ck, mrq)
	case "fleas":
		return fleas.Process(ck, mrq)
	case "ranking":
		return ranking.Process(ck, mrq)
	case "daily":
		return daily.Process(ck, mrq)
	case "acc":
		return acc.Process(ck, mrq)
	case "settings":
		return settings.Process(ck, mrq)
	default:
		panic(fmt.Sprintf("Value of module ('%v') is not valid", module))
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
		data := cryp.Decryp(key, rq[1:])
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

	js := cryp.Decryp(comKey, rest)
	data := json.FromString(js).Ro()
	return hub(comKey, data)
}
