// Copyright 20-Aug-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package balance

import (
	"github.com/dedeme/golib/file"
	"github.com/dedeme/golib/json"
	"path"
)

// Create year balance data base. 'dpath' is for expample "/years/2021".
func Mk(dpath string) {
	Write(dpath, json.Wd(0.0))
}

// 'dpath' is for expample "/years/2021".
func Read(dpath string) json.T {
	return json.FromString(file.ReadAll(path.Join(dpath, "balance.tb")))
}

// 'dpath' is for expample "/years/2021".
func Write(dpath string, value json.T) {
	file.WriteAll(path.Join(dpath, "balance.tb"), value.String())
}
