// Copyright 10-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Global ranking table.
package rankingTb

import (
	"github.com/dedeme/MultiMarket/data/flea/irank"
	"github.com/dedeme/MultiMarket/global/sync"
	"github.com/dedeme/golib/file"
	"github.com/dedeme/golib/json"
	"path"
)

var fpath string

// Initializes ranking.
//    parent: Parent directory of "Ranking.tb".
func Initialize(lk sync.T, parent string) {
	fpath = path.Join(parent, "Ranking.tb")
	if !file.Exists(fpath) {
		Write(lk, []*irank.T{})
	}
}

// Writes an investors ranking.
//    lk    : Synchronization lock.
//    iranks: Investors rankings (sorted from after to before).
func Write(lk sync.T, iranks []*irank.T) {
	var jss []json.T
	for _, e := range iranks {
		jss = append(jss, e.ToJs())
	}
	file.WriteAll(fpath, json.Wa(jss).String())
}

// Reads investors rankings (sorted from after to before).
//    lk: Synchronization lock.
func Read(lk sync.T) (iranks []*irank.T) {
	for _, e := range json.FromString(file.ReadAll(fpath)).Ra() {
		iranks = append(iranks, irank.FromJs(e))
	}
	return
}
