// Copyright 05-Oct-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Directory row widget.
package dirRow

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
	case "showDirs":
		id := js.Rs(mrq["id"])
		poolDb.ShowDirs(id)
		return cgi.RpEmpty(ck)
	case "copyToBase":
		id := js.Rs(mrq["id"])
		if !globals.IsBusy {
			globals.IsBusy = true
			poolDb.CopyToBase(id)
			globals.IsBusy = false
		}
		return cgi.RpEmpty(ck)
	case "copyFromBase":
		id := js.Rs(mrq["id"])
		if !globals.IsBusy {
			globals.IsBusy = true
			poolDb.CopyFromBase(id)
			globals.IsBusy = false
		}
		return cgi.RpEmpty(ck)
	case "createPathTxt":
		id := js.Rs(mrq["id"])
		if !globals.IsBusy {
			globals.IsBusy = true
			poolDb.CreatePathTxt(id)
			globals.IsBusy = false
		}
		return cgi.RpEmpty(ck)
	case "changeDir":
		id := js.Rs(mrq["id"])
		newId := js.Rs(mrq["newId"])
		if !globals.IsBusy {
			globals.IsBusy = true
			poolDb.ChangeDir(id, newId)
			globals.IsBusy = false
		}
		return cgi.RpEmpty(ck)
	case "changePath":
		id := js.Rs(mrq["id"])
		newPath := js.Rs(mrq["newPath"])
		if !globals.IsBusy {
			globals.IsBusy = true
			poolDb.ChangePath(id, newPath)
			globals.IsBusy = false
		}
		return cgi.RpEmpty(ck)
	case "changeBig":
		id := js.Rs(mrq["id"])
		if !globals.IsBusy {
			globals.IsBusy = true
			poolDb.ChangeBig(id)
			globals.IsBusy = false
		}
		return cgi.RpEmpty(ck)
	case "update":
		id := js.Rs(mrq["id"])
		if !globals.IsBusy {
			globals.IsBusy = true
			poolDb.UpdateDir(id)
			globals.IsBusy = false
		}
		return cgi.RpEmpty(ck)
	case "delete":
		id := js.Rs(mrq["id"])
		if !globals.IsBusy {
			globals.IsBusy = true
			poolDb.Delete(id)
			globals.IsBusy = false
		}
		return cgi.RpEmpty(ck)
	default:
		panic(fmt.Sprintf("Value of rq ('%v') is not valid", rq))
	}
}
