// Copyright 02-Sep-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package budget

import (
	"github.com/dedeme/golib/file"
	"github.com/dedeme/golib/json"
	"path"
)

// Returns an empty table.
func mkEmpty() json.T {
	var months []json.T
	for i := 0; i < 12; i++ {
		months = append(months, json.Wa([]json.T{}))
	}
	return json.Wa(months)
}

// Create year budget data base. 'dpath' is for expample "/years/2021".
func Mk(dpath string) {
	Write(dpath, mkEmpty())
}

// 'dpath' is for expample "/years/2021".
// If 'dpath' does not exists, it returns an empty table.
func Read(dpath string) json.T {
	if file.Exists(dpath) {
		return json.FromString(file.ReadAll(path.Join(dpath, "budget.tb")))
	}
	return mkEmpty()
}

// 'dpath' is for expample "/years/2021".
func Write(dpath string, entries json.T) {
	file.WriteAll(path.Join(dpath, "budget.tb"), entries.String())
}
