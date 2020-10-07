// Copyright 08-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Last daily quotes table.
package dailyTb

import (
	"github.com/dedeme/MultiMarket/data/nick"
	"github.com/dedeme/MultiMarket/global/sync"
	"github.com/dedeme/golib/file"
	"github.com/dedeme/golib/json"
	"path"
)

var fpath string

// Initializes daily server table.
func Initialize(lk sync.T, parent string) {
	fpath = path.Join(parent, "Daily.tb")
	if !file.Exists(fpath) {
		Write(lk, []*nick.QvalueT{})
	}
}

// Reads daily quotes.
//    lk: Synchronization lock.
func Read(lk sync.T) (r []*nick.QvalueT) {
	for _, e := range json.FromString(file.ReadAll(fpath)).Ra() {
		r = append(r, nick.QvalueFromJs(e))
	}
	return
}

// Writes daily quotes.
//    lk: Synchronization lock.
//    qs: daily quotes.
func Write(lk sync.T, qs []*nick.QvalueT) {
	var a []json.T
	for _, e := range qs {
		a = append(a, e.ToJs())
	}
	file.WriteAll(fpath, json.Wa(a).String())
}
