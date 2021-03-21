// Copyright 23-Aug-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Unsorted performance table.
package performanceTb

import (
	"github.com/dedeme/MultiMarket/data/cts"
	"github.com/dedeme/MultiMarket/data/performance"
	"github.com/dedeme/MultiMarket/global/sync"
	"github.com/dedeme/golib/file"
	"github.com/dedeme/golib/json"
	"path"
)

// Auxiliar struct --------------------------------------------------------- END

var fpath string

// Initializes unsorted table.
//    parent: Parent directory.
func Initialize(lk sync.T, parent string) {
	fpath = path.Join(parent, "Performance.tb")
	if !file.Exists(fpath) {
		Write(lk, []*performance.T{})
	}
}

// Returns data in JSON format.
//    lk: Synchronization lock.
func ReadJs(lk sync.T) json.T {
	return json.FromString(file.ReadAll(fpath))
}

// Returns data.
//    lk: Synchronization lock.
func Read(lk sync.T) []*performance.T {
	var r []*performance.T
	for _, rc := range ReadJs(lk).Ra() {
		r = append(r, performance.FromJs(rc))
	}
	return r
}

// Writes data.
//    lk  : Synchronization lock.
//    data: Performance records
func Write(lk sync.T, data []*performance.T) {
	var a []json.T
	for _, rc := range data {
		a = append(a, rc.ToJs())
	}
	toRemove := len(a) - cts.PerformanceMax
	if toRemove < 0 {
		a = a[toRemove:]
	}
	file.WriteAll(fpath, json.Wa(a).String())
}
