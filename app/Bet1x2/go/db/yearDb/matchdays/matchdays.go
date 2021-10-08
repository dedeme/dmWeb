// Copyright 14-Sep-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Matchdays table.
package matchdays

import (
	"github.com/dedeme/Bet1x2/data/cts"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/file"
	"github.com/dedeme/golib/json"
	"path"
)

func fpath(year string) string {
	return path.Join(cgi.Home(), cts.DataPath, year, "matchdays.tb")
}

// Create year clubs table.
func Mk(year string) {
	Write(fpath(year), json.Wa([]json.T{}))
}

func Read(year string) json.T {
	return json.FromString(file.ReadAll(fpath(year)))
}

func Write(year string, entries json.T) {
	file.WriteAll(fpath(year), entries.String())
}
