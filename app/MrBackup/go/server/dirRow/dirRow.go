// Copyright 05-Oct-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Directory row widget.
package dirRow

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
	case "showDirs":
		id := cgi.RqString(mrq, "id")
		poolDb.ShowDirs(id)
		return cgi.RpEmpty(ck)
	case "copyToBase":
		id := cgi.RqString(mrq, "id")
		if !globals.IsBusy {
			globals.IsBusy = true
			poolDb.CopyToBase(id)
			globals.IsBusy = false
		}
		return cgi.RpEmpty(ck)
	case "copyFromBase":
		id := cgi.RqString(mrq, "id")
		if !globals.IsBusy {
			globals.IsBusy = true
			poolDb.CopyFromBase(id)
			globals.IsBusy = false
		}
		return cgi.RpEmpty(ck)
	case "createPathTxt":
		id := cgi.RqString(mrq, "id")
		if !globals.IsBusy {
			globals.IsBusy = true
			poolDb.CreatePathTxt(id)
			globals.IsBusy = false
		}
		return cgi.RpEmpty(ck)
	case "changeDir":
		id := cgi.RqString(mrq, "id")
		newId := cgi.RqString(mrq, "newId")
		if !globals.IsBusy {
			globals.IsBusy = true
			poolDb.ChangeDir(id, newId)
			globals.IsBusy = false
		}
		return cgi.RpEmpty(ck)
	case "changePath":
		id := cgi.RqString(mrq, "id")
		newPath := cgi.RqString(mrq, "newPath")
		if !globals.IsBusy {
			globals.IsBusy = true
			poolDb.ChangePath(id, newPath)
			globals.IsBusy = false
		}
		return cgi.RpEmpty(ck)
  case "changeBig":
		id := cgi.RqString(mrq, "id")
		if !globals.IsBusy {
			globals.IsBusy = true
			poolDb.ChangeBig(id)
			globals.IsBusy = false
		}
		return cgi.RpEmpty(ck)
	case "update":
		id := cgi.RqString(mrq, "id")
		if !globals.IsBusy {
			globals.IsBusy = true
			poolDb.UpdateDir(id)
			globals.IsBusy = false
		}
		return cgi.RpEmpty(ck)
	case "delete":
		id := cgi.RqString(mrq, "id")
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
