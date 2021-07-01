// Copyright 27-Jun-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// StocksPage javascript page
package stocks

import (
	"github.com/dedeme/golib/date"
	"github.com/dedeme/golib/file"
	"github.com/dedeme/golib/json"
	"path"
)

const hcontaPath = "/dm/wwwcgi/dmcgi/Hconta/data"
const cashAcc = "57202"
const stocksAcc = "54000"

func hcontaData() (cash, sum float64) {
	fdb := path.Join(hcontaPath, date.Now().Format("%Y")+".db")
	if !file.Exists(fdb) {
		panic(fdb + " not found")
	}
	accJs := json.FromString(file.ReadAll(fdb)).Ra()[3]
	for _, eJs := range accJs.Ra() {
		fields := eJs.Ra()
		debitsJs := fields[2]
		creditsJs := fields[3]

		if ammJs, ok := debitsJs.Ro()[cashAcc]; ok {
			cash += ammJs.Rd()
		}
		if ammJs, ok := debitsJs.Ro()[stocksAcc]; ok {
			sum += ammJs.Rd()
		}

		if ammJs, ok := creditsJs.Ro()[cashAcc]; ok {
			cash -= ammJs.Rd()
		}
		if ammJs, ok := creditsJs.Ro()[stocksAcc]; ok {
			sum -= ammJs.Rd()
		}
	}
	return
}
