// Copyright 20-Aug-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package balance

import (
	"github.com/dedeme/ktlib/file"
	"github.com/dedeme/ktlib/js"
	"github.com/dedeme/ktlib/path"
)

// Create year balance data base. 'dpath' is for expample "/years/2021".
func Mk(dpath string) {
	Write(dpath, js.Wd(0.0))
}

// 'dpath' is for expample "/years/2021".
func Read(dpath string) string {
	return file.Read(path.Cat(dpath, "balance.tb"))
}

// 'dpath' is for expample "/years/2021".
func Write(dpath string, value string) {
	file.Write(path.Cat(dpath, "balance.tb"), value)
}
