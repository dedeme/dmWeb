// Copyright 20-Oct-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Nicks list page.
package list

import (

)

func Process(ck string, mrq map[string]json.T) string {
	rq := cgi.RqString(mrq, "rq")
	switch rq {
	case "download":
		nickId := cgi.RqInt(mrq, "nickId")
		rp := map[string]json.T{}
		result := ""
		lock.Run(func() {
			nk, ok := nicksTb.Read().NickFromId(nickId)
			if !ok {
				logTb.Error(fmt.Sprintf(
					"List/download: Nick %v not found", nickId,
				))
				result = "error"
				return
			}
			warnings, err := net.UpdateHistoric(nk)
			if err != nil {
				logTb.Error("List/download:\n" + err.Error())
				result = "error"
				return
			}
			if len(warnings) != 0 {
				msg := "List/download:"
				for _, w := range warnings {
					msg += "\n" + w.Error()
				}
				logTb.Info(msg)
				result = "warnings"
			}
		})
		rp["result"] = json.Ws(result)
		return cgi.Rp(ck, rp)
	case "test":
		nickName := cgi.RqString(mrq, "nickName")
		result := ""
		rp := map[string]json.T{}
		lock.Run(func() {
			qs, err := quotesDb.Read(nickName)
			if err != nil {
				logTb.Error("List/test:\n" + err.Error())
				result = "error"
				return
			}
			_, withWarnings, withError := quotesDb.Check(nickName, qs)
			if withWarnings {
				result = "warnings"
			} else if withError {
				result = "error"
			}
		})
		rp["result"] = json.Ws(result)
		return cgi.Rp(ck, rp)
	default:
		panic(fmt.Sprintf("Value of rq ('%v') is not valid", rq))
	}
}
