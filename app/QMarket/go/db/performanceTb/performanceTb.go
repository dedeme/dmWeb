// Copyright 13-Nov-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Unsorted performance table.
package performanceTb

import (
	"github.com/dedeme/QMarket/data/cts"
	"github.com/dedeme/QMarket/data/performance"
	"github.com/dedeme/golib/file"
	"github.com/dedeme/golib/json"
	"path"
)

// Auxiliar struct --------------------------------------------------------- END

var fpath string

// Initializes unsorted table.
//    parent: Parent directory.
func Initialize(parent string) {
	fpath = path.Join(parent, "Performance.tb")
	if !file.Exists(fpath) {
		Write([]*performance.T{})
	}
}

// Returns data in JSON format.
//    lk: Synchronization lock.
func ReadJs() json.T {
	return json.FromString(file.ReadAll(fpath))
}

// Returns data.
//    lk: Synchronization lock.
func Read() []*performance.T {
	var r []*performance.T
	for _, rc := range ReadJs().Ra() {
		r = append(r, performance.FromJs(rc))
	}
	return r
}

// Writes data.
//    lk  : Synchronization lock.
//    data: Performance records
func Write(data []*performance.T) {
	var a []json.T
	for _, rc := range data {
		a = append(a, rc.ToJs())
	}
	toRemove := len(a) - cts.PerformanceMax
	if toRemove > 0 {
		a = a[toRemove:]
	}
	file.WriteAll(fpath, json.Wa(a).String())
}
