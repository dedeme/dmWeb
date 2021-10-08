// Copyright 14-Sep-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// All the clubs table.
package allClubs

import (
	"github.com/dedeme/Bet1x2/data/cts"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/file"
	"github.com/dedeme/golib/json"
	"path"
)

func fpath() string {
	return path.Join(cgi.Home(), cts.DataPath, "allClubs.tb")
}

// Create year clubs table.
func Mk() {
	Write(json.Wa([]json.T{}))
}

func Read() json.T {
	return json.FromString(file.ReadAll(fpath()))
}

func Write(entries json.T) {
	file.WriteAll(fpath(), entries.String())
}
