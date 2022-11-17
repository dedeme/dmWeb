// Copyright 03-Jun-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Web server
package server

import (
	"github.com/dedeme/KtMarket/cts"
	"github.com/dedeme/KtMarket/db"
	"github.com/dedeme/KtMarket/server/pgs/acc"
	"github.com/dedeme/KtMarket/server/pgs/daily"
	"github.com/dedeme/KtMarket/server/pgs/home"
	"github.com/dedeme/KtMarket/server/pgs/models"
	"github.com/dedeme/KtMarket/server/pgs/settings"
	"github.com/dedeme/KtMarket/server/pgs/wgs"
	"github.com/dedeme/ktlib/cgi"
	"github.com/dedeme/ktlib/cryp"
	"github.com/dedeme/ktlib/js"
	"github.com/dedeme/ktlib/str"
	"github.com/dedeme/ktlib/sys"
	"github.com/dedeme/ktlib/thread"
	"github.com/dedeme/ktlib/websv"
  "github.com/dedeme/ktlib/log"
)

func Start() {
	websv.Start(
		cts.Port,
		0,
		"/dm/dmWeb/app/KtMarket/hx",
		cryp.Encode(cryp.Key(cts.AppName, 100), cryp.Key("end", 100)),
		func(rq string) string {
			// GET

			rp, ok := websv.GetRq(rq)

			if ok {
				return rp
			}

			// POST

			rqUnix := str.Replace(rq, "\r", "")
			ix := str.Index(rq, "\n")
			if ix == -1 {
				return websv.BadRequestRp(rq)
			}

			rq = str.Trim(rq[:ix])
			if rq != "POST /cgi-bin/ccgi.sh HTTP/1.1" {
				return websv.BadRequestRp("Bad POST request\n" + rq)
			}

			ix = str.Index(rqUnix, "\n\n")
			if ix == -1 {
				return websv.BadRequestRp(rq)
			}

			rqUnix = str.Trim(rqUnix[ix+2:])
			ix = str.Index(rqUnix, ":")
			if ix == -1 {
				return websv.BadRequestRp(rq)
			}

			cmd := rqUnix[:ix]
			par := rqUnix[ix+1:]
			if cmd != cts.AppName {
				return websv.BadRequestRp("Unauthorized source '" + cmd + "'")
			}

			return websv.TextRp(rqProcess(par))
		},
    log.Error,
	)
}

func rqProcess(rq string) (rp string) {
	defer func() {
		if err := recover(); err != nil {
			rp = sys.Fail(str.Fmt("%v", err))
			ix1 := str.Index(rp, ":\n")
			if ix1 != -1 {
				ix2 := str.IndexFrom(rp, "runtime/panic.go", ix1)
				if ix2 != -1 {
					ix2 = str.IndexFrom(rp, "\n", ix2)
				}
				if ix2 != -1 {
					rp = rp[:ix1+2] + rp[ix2+1:]
				}
			}
		}
	}()

	ix := str.Index(rq, ":")

	// CONNECTION --------------------------------------------------------------

	if ix == -1 {
		return cgi.Connect(rq)
	}

	// AUTHENTICATION ----------------------------------------------------------

	if ix == 0 {
		key := cryp.Key(cts.AppName, cgi.Klen)
		data := cryp.Decode(key, rq[1:])
		ps := str.Split(data, ":")
		return cgi.Authentication(key, ps[0], ps[1], ps[2] == "1")
	}

	// NORMAL DATA -------------------------------------------------------------

	sessionId := rq[:ix]
	var conKey string
	rest := rq[ix+1:]
	ix = str.Index(rest, ":")
	if ix != -1 {
		conKey = rest[:ix]
		rest = rest[ix+1:]
	}
	comKey, ok := cgi.GetComKey(sessionId, conKey)
	if ok {
		j := cryp.Decode(comKey, rest)
		data := js.Ro(j)
		return processHub(comKey, data)
	} else {
		return cgi.RpExpired()
	}
}

func processHub(ck string, rq cgi.T) string {
	module := js.Rs(rq["module"])
	switch module {
	case "main":
		return processMain(ck, rq)
	case "home":
		return home.Process(ck, rq)
	case "models":
		return models.Process(ck, rq)
	case "acc":
		return acc.Process(ck, rq)
	case "daily":
		return daily.Process(ck, rq)
	case "settings":
		return settings.Process(ck, rq)
	case "wgs":
		return wgs.Process(ck, rq)
	default:
		panic("Value of module (" + module + ") is not valid")
	}
}

func processMain(ck string, mrq cgi.T) string {
	rq := js.Rs(mrq["rq"])
	switch rq {
	case "lang":
		{
			var lang string
			thread.Sync(func() {
				lang = db.ConfTb().Read().Lang
			})
			return cgi.Rp(ck, cgi.T{
				"lang": js.Ws(lang),
			})
		}
	default:
		panic("Value of rq (" + rq + ") is not valid")
	}
}
