// Copyright 02-Sep-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package budget

import (
	"github.com/dedeme/ktlib/file"
	"github.com/dedeme/ktlib/js"
	"github.com/dedeme/ktlib/path"
)

// Returns an empty table.
func mkEmpty() string {
	var months []string
	for i := 0; i < 12; i++ {
		months = append(months, js.Wa([]string{}))
	}
	return js.Wa(months)
}

// Create year budget data base. 'dpath' is for expample "/years/2021".
func Mk(dpath string) {
	Write(dpath, mkEmpty())
}

// 'dpath' is for expample "/years/2021".
// If 'dpath' does not exists, it returns an empty table.
func Read(dpath string) string {
	if file.Exists(dpath) {
		return file.Read(path.Cat(dpath, "budget.tb"))
	}
	return mkEmpty()
}

// 'dpath' is for expample "/years/2021".
func Write(dpath string, entries string) {
	file.Write(path.Cat(dpath, "budget.tb"), entries)
}
