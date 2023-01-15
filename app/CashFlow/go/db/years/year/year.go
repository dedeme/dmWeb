// Copyright 25-Aug-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Year data base initialization
package year

import (
	"github.com/dedeme/CashFlow/db/years/year/balance"
	"github.com/dedeme/CashFlow/db/years/year/budget"
	"github.com/dedeme/CashFlow/db/years/year/diary"
	"github.com/dedeme/CashFlow/db/years/year/plan"
	"github.com/dedeme/ktlib/file"
)

// Create year data base. 'dpath' is for expample "/years/2021"
func Mk(dpath string) {
	file.Mkdir(dpath)
	plan.Mk(dpath)
	balance.Mk(dpath)
	diary.Mk(dpath)
	budget.Mk(dpath)
}

func MkFrom(sourcePath string, dpath string) {
	file.Mkdir(dpath)
	plan.Write(dpath, plan.Read(sourcePath))
	balance.Mk(dpath)
	diary.Mk(dpath)
	budget.Write(dpath, budget.Read(sourcePath))
}
