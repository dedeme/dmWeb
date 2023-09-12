// Copyright 05-Oct-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Summary page.
package summary

import (
	"fmt"
	"github.com/dedeme/MrBackup/data/globals"
	"github.com/dedeme/MrBackup/db/log"
	"github.com/dedeme/MrBackup/db/poolDb"
	"github.com/dedeme/ktlib/cgi"
	"github.com/dedeme/ktlib/js"
)

func Process(ck string, mrq map[string]string) string {
	rq := js.Rs(mrq["rq"])
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
		rp := map[string]string{
			"isBusy":   js.Wb(isBusy),
			"pools":    js.Wi(pools),
			"badPools": js.Wi(badPools),
			"dirs":     js.Wi(dirs),
			"badDirs":  js.Wi(badDirs),
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
		rp := map[string]string{
			"isBusy":        js.Wb(isBusy),
			"files":         js.Wi(files),
			"outdatedDirs":  js.Wi(outdatedDirs),
			"outdatedFiles": js.Wi(outdatedFiles),
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
		rp := map[string]string{"isBusy": js.Wb(globals.IsBusy)}
		return cgi.Rp(ck, rp)
	case "log":
		rp := map[string]string{
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
