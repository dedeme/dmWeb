// Copyright 14-Sep-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// All the clubs table.
package allClubs

import (
	"github.com/dedeme/golib/file"
	"github.com/dedeme/golib/json"
	"path"
)

// Create year clubs table. 'dpath' is the Data directory.
func Mk(dpath string) {
	Write(dpath, json.Wa([]json.T{}))
}

// 'dpath' is for expample ".../2021".
func Read(dpath string) json.T {
	return json.FromString(file.ReadAll(path.Join(dpath, "allClubs.tb")))
}

// 'dpath' is for expample ".../2021".
func Write(dpath string, entries json.T) {
	file.WriteAll(path.Join(dpath, "allClubs.tb"), entries.String())
}

