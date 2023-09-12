// Copyright 05-Oct-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Directories page.
package directories

import (
	"fmt"
	"github.com/dedeme/MrBackup/data/globals"
	"github.com/dedeme/MrBackup/db/poolDb"
	"github.com/dedeme/ktlib/cgi"
	"github.com/dedeme/ktlib/js"
)

func Process(ck string, mrq map[string]string) string {
	rq := js.Rs(mrq["rq"])
	switch rq {
	case "idata":
		isBusy := globals.IsBusy
		var data string
		if !isBusy {
			globals.IsBusy = true
			mapRs := poolDb.GlobalTest()
			mapJs := map[string]string{}
			for k, v := range mapRs {
				mapJs[k] = v.ToJs()
			}
			data = js.Wo(mapJs)
			globals.IsBusy = false
		}
		rp := map[string]string{
			"isBusy": js.Wb(isBusy),
			"data":   data,
		}
		return cgi.Rp(ck, rp)
	case "new":
		id := js.Rs(mrq["id"])
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
