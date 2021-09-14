// Copyright 26-Aug-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package diary

import (
	"github.com/dedeme/golib/file"
	"github.com/dedeme/golib/json"
	"path"
)

// Create year annotations data base. 'dpath' is for expample "/years/2021".
func Mk(dpath string) {
	Write(dpath, json.Wa([]json.T{}))
}

// 'dpath' is for expample "/years/2021".
func Read(dpath string) json.T {
	return json.FromString(file.ReadAll(path.Join(dpath, "diary.tb")))
}

// 'dpath' is for expample "/years/2021".
func Write(dpath string, entries json.T) {
	file.WriteAll(path.Join(dpath, "diary.tb"), entries.String())
}
