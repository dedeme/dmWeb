// Copyright 16-Oct-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Main hub to process client request.
package mainHub

import (
	"fmt"
	"github.com/dedeme/QMarket/data/cts"
	"github.com/dedeme/QMarket/data/log"
	"github.com/dedeme/QMarket/db/confTb"
	"github.com/dedeme/QMarket/lock"
	"github.com/dedeme/QMarket/server/pgs/acc"
	"github.com/dedeme/QMarket/server/pgs/daily"
	"github.com/dedeme/QMarket/server/pgs/home"
	"github.com/dedeme/QMarket/server/pgs/models"
	"github.com/dedeme/QMarket/server/pgs/performance"
	"github.com/dedeme/QMarket/server/pgs/ranking"
	"github.com/dedeme/QMarket/server/pgs/settings"
	"github.com/dedeme/QMarket/server/pgs/wgs"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/cryp"
	"github.com/dedeme/golib/json"
	"strings"
)

func subHub(ck string, mrq map[string]json.T) string {
	rq := cgi.RqString(mrq, "rq")
	switch rq {
	case "lang":
		rp := map[string]json.T{}
		lock.Run(func() { rp["lang"] = json.Ws(confTb.Lang()) })
		return cgi.Rp(ck, rp)
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
	case "models":
		return models.Process(ck, mrq)
	case "performance":
		return performance.Process(ck, mrq)
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
			rp = log.Format(fmt.Sprint(r))
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
