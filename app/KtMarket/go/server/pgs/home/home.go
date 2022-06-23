// Copyright 03-Jun-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Home page
package home

import (
	"github.com/dedeme/ktlib/cgi"
	"github.com/dedeme/ktlib/js"
	"github.com/dedeme/ktlib/log"
	"github.com/dedeme/ktlib/thread"
)

func Process(ck string, mrq cgi.T) string {
	rq := js.Rs(mrq["rq"])
	switch rq {
	case "log":
		var jLog string
		thread.Sync(func() {
			jLog = log.ReadJs()
		})
		return cgi.Rp(ck, cgi.T{
			"log": jLog,
		})
	case "resetLog":
		thread.Sync(func() {
			log.Reset()
		})
		return cgi.RpEmpty(ck)
	default:
		panic("Value of rq (" + rq + ") is not valid")
	}
}
