// Copyright 27-Jun-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// StocksPage javascript page
package stocks

import (
	"github.com/dedeme/ktlib/time"
	"github.com/dedeme/ktlib/file"
	"github.com/dedeme/ktlib/js"
	"github.com/dedeme/ktlib/path"
)

const hcontaPath = "/dm/wwwcgi/dmcgi/Hconta/data"
const cashAcc = "57202"
const stocksAcc = "54000"

func hcontaData() (cash, sum float64) {
	fdb := path.Cat(hcontaPath, time.Fmt("%Y", time.Now())+".db")
	if !file.Exists(fdb) {
		panic(fdb + " not found")
	}
	accJs := js.Ra(file.Read(fdb))[3]
	for _, eJs := range js.Ra(accJs) {
		fields := js.Ra(eJs)
		debitsJs := fields[2]
		creditsJs := fields[3]

		if ammJs, ok := js.Ro(debitsJs)[cashAcc]; ok {
			cash += js.Rd(ammJs)
		}
		if ammJs, ok := js.Ro(debitsJs)[stocksAcc]; ok {
			sum += js.Rd(ammJs)
		}

		if ammJs, ok := js.Ro(creditsJs)[cashAcc]; ok {
			cash -= js.Rd(ammJs)
		}
		if ammJs, ok := js.Ro(creditsJs)[stocksAcc]; ok {
			sum -= js.Rd(ammJs)
		}
	}
	return
}
