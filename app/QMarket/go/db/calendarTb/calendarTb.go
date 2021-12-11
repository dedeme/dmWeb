// Copyright 16-Oct-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Global state data base.
package calendarTb

import (
	"github.com/dedeme/QMarket/data/calendar"
	"github.com/dedeme/golib/file"
	"github.com/dedeme/golib/json"
	"path"
)

var fpath string

// Intializes table.
//    parent: Parent directory of "Conf.tb".
func Initialize(parent string) {
	fpath = path.Join(parent, "Calendar.tb")
	if !file.Exists(fpath) {
		Write(calendar.New())
	}
}

func ReadJs() json.T {
	return json.FromString(file.ReadAll(fpath))
}

func Read() *calendar.T {
	return calendar.FromJs(ReadJs())
}

func Write(c *calendar.T) {
	file.WriteAll(fpath, c.ToJs().String())
}
