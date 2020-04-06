// Copyright 30-Mar-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Main hub.
package hub

import (
	"fmt"
	"github.com/dedeme/MMarket/data/cts"
	"github.com/dedeme/MMarket/db/log"
	"github.com/dedeme/MMarket/server/homeMod"
	"github.com/dedeme/MMarket/server/mainMod"
	"github.com/dedeme/MMarket/server/settingsMod/settingsHub"
	"github.com/dedeme/MMarket/server/fleasMod/fleasHub"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/cryp"
	"github.com/dedeme/golib/json"
	"github.com/dedeme/golib/sys"
	"path"
	"runtime/debug"
	"strings"
)

func moduleProcess(ac chan func(), ck string, mrq map[string]json.T) string {
	key := cgi.RqString(mrq, "module")
	switch key {
	case "Main":
		return mainmod.Process(ac, ck, mrq)
	case "Home":
		return home.Process(ac, ck, mrq)
	case "Settings":
		return settingsHub.Process(ac, ck, mrq)
	case "Fleas":
		return fleasHub.Process(ac, ck, mrq)
	default:
		panic(fmt.Sprintf("Value of key is '%v', but it must be '%v'",
			key, "Main | Home | Settings"))
	}
}

func Process(ac chan func(), rq string) (rp string) {
	defer func() {
		if err := recover(); err != nil {
			e := fmt.Sprint(err)
			log.Error(e)
			rp = fmt.Sprintf("%v\n%v", e, string(debug.Stack()))
		}
	}()

	cgi.Initialize(path.Join(path.Dir(sys.Home()), cts.APP_NAME), 900)

	ix := strings.IndexByte(rq, ':')
	//............................................................... CONNECTION
	if ix == -1 {
		return cgi.Connect(rq)
	}

	//........................................................... AUTHENTICATION
	if ix == 0 {
		key := cryp.Key(cts.APP_NAME, cgi.Klen)
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
	return moduleProcess(ac, comKey, data)
}
