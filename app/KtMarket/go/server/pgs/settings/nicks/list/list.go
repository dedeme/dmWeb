// Copyright 20-Oct-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Nicks list page.
package list

import (
	"github.com/dedeme/KtMarket/db"
	"github.com/dedeme/KtMarket/net"
	"github.com/dedeme/ktlib/cgi"
	"github.com/dedeme/ktlib/js"
	"github.com/dedeme/ktlib/log"
	"github.com/dedeme/ktlib/str"
	"github.com/dedeme/ktlib/thread"
)

func Process(ck string, mrq cgi.T) string {
	rq := js.Rs(mrq["rq"])
	switch rq {
	case "download":
		nickId := js.Ri(mrq["nickId"])

		result := ""
		thread.Sync(func() {
			nk, ok := db.NicksTb().Read().NickFromId(nickId)
			if !ok {
				log.Error(str.Fmt("List/download: Nick %v not found", nickId))
				result = "error"
				return
			}
			warnings, err := net.UpdateHistoric(nk)
			if err != "" {
				log.Error("List/download (" + nk.Name + "):\n" + err)
				result = "error"
				return
			}
			if len(warnings) != 0 {
				msg := "List/download (" + nk.Name + ":"
				for _, w := range warnings {
					msg += "\n" + w
				}
				log.Warning(msg)
				result = "warnings"
			}
		})

		return cgi.Rp(ck, cgi.T{
			"result": js.Ws(result),
		})

	case "test":
		nickName := js.Rs(mrq["nickName"])

		result := ""

		thread.Sync(func() {
			qs, err := db.QsRead(nickName)
			if err != "" {
				log.Error("List/test (" + nickName + ":\n" + err)
				result = "error"
				return
			}
			_, withWarnings, withError := db.QsCheck(nickName, qs)
			if withWarnings {
				result = "warnings"
			} else if withError {
				result = "error"
			}
		})

		return cgi.Rp(ck, cgi.T{
			"result": js.Ws(result),
		})

	default:
		panic("Value of rq ('" + rq + "') is not valid")
	}
}
