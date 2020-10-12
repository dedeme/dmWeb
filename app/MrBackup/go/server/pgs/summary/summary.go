// Copyright 05-Oct-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Summary page.
package summary

import (
	"fmt"
	"github.com/dedeme/MrBackup/data/globals"
	"github.com/dedeme/MrBackup/db/log"
	"github.com/dedeme/MrBackup/db/poolDb"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/json"
)

func Process(ck string, mrq map[string]json.T) string {
	rq := cgi.RqString(mrq, "rq")
	switch rq {
	case "dirs":
		isBusy := globals.IsBusy
		var pools int
		var badPools int
		var dirs int
		var badDirs int
		if !isBusy {
			globals.IsBusy = true
			pools, badPools, dirs, badDirs = poolDb.TestDirs()
			globals.IsBusy = false
		}
		rp := map[string]json.T{
			"isBusy":   json.Wb(isBusy),
			"pools":    json.Wi(pools),
			"badPools": json.Wi(badPools),
			"dirs":     json.Wi(dirs),
			"badDirs":  json.Wi(badDirs),
		}
		return cgi.Rp(ck, rp)
	case "files":
		isBusy := globals.IsBusy
		var files int
		var outdatedDirs int
		var outdatedFiles int
		if !isBusy {
			globals.IsBusy = true
			files, outdatedDirs, outdatedFiles = poolDb.TestFiles()
			globals.IsBusy = false
		}
		rp := map[string]json.T{
			"isBusy":        json.Wb(isBusy),
			"files":         json.Wi(files),
			"outdatedDirs":  json.Wi(outdatedDirs),
			"outdatedFiles": json.Wi(outdatedFiles),
		}
		return cgi.Rp(ck, rp)
	case "update":
		if !globals.IsBusy {
			go func() {
				globals.IsBusy = true
				poolDb.Update()
				globals.IsBusy = false
			}()
		}
		return cgi.RpEmpty(ck)
	case "isBusy":
		rp := map[string]json.T{"isBusy": json.Wb(globals.IsBusy)}
		return cgi.Rp(ck, rp)
	case "log":
		rp := map[string]json.T{
			"log": log.Read(),
		}
		return cgi.Rp(ck, rp)
	case "resetLog":
		log.Reset()
		return cgi.RpEmpty(ck)
	default:
		panic(fmt.Sprintf("Value of rq ('%v') is not valid", rq))
	}
}
