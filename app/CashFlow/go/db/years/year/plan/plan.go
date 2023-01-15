// Copyright 26-Aug-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package plan

import (
	"github.com/dedeme/ktlib/file"
	"github.com/dedeme/ktlib/js"
	"github.com/dedeme/ktlib/path"
)

// Create year annotations data base. 'dpath' is for expample "/years/2021".
func Mk(dpath string) {
	Write(dpath, js.Wa([]string{}))
}

// 'dpath' is for expample "/years/2021".
func Read(dpath string) string {
	return file.Read(path.Cat(dpath, "plan.tb"))
}

// 'dpath' is for expample "/years/2021".
func Write(dpath string, entries string) {
	file.Write(path.Cat(dpath, "plan.tb"), entries)
}
