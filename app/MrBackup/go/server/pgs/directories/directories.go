// Copyright 05-Oct-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Directories page.
package directories

import (
	"fmt"
	"github.com/dedeme/MrBackup/data/globals"
	"github.com/dedeme/MrBackup/db/poolDb"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/json"
)

func Process(ck string, mrq map[string]json.T) string {
	rq := cgi.RqString(mrq, "rq")
	switch rq {
	case "idata":
		isBusy := globals.IsBusy
		var data json.T
		if !isBusy {
			globals.IsBusy = true
			mapRs := poolDb.GlobalTest()
			mapJs := map[string]json.T{}
			for k, v := range mapRs {
				mapJs[k] = v.ToJs()
			}
			data = json.Wo(mapJs)
			globals.IsBusy = false
		}
		rp := map[string]json.T{
			"isBusy": json.Wb(isBusy),
			"data":   data,
		}
		return cgi.Rp(ck, rp)
	case "new":
		id := cgi.RqString(mrq, "id")
		if !globals.IsBusy {
			globals.IsBusy = true
			poolDb.AddDir(id)
			globals.IsBusy = false
		}
		return cgi.RpEmpty(ck)
	default:
		panic(fmt.Sprintf("Value of rq ('%v') is not valid", rq))
	}
}
