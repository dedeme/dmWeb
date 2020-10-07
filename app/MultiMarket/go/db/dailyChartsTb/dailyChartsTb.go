// Copyright 16-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Daily charts data table.
package dailyChartsTb

import (
	"github.com/dedeme/MultiMarket/data/dailyChart"
	"github.com/dedeme/MultiMarket/global/sync"
	"github.com/dedeme/golib/file"
	"github.com/dedeme/golib/json"
	"path"
)

var fpath string

// Initializes charts data table.
//    parent: Parent directory.
func Initialize(lk sync.T, parent string) {
	fpath = path.Join(parent, "DailyChart.tb")
	if !file.Exists(fpath) {
		Write(lk, []*dailyChart.T{})
	}
}

func ReadJs(lk sync.T) json.T {
	return json.FromString(file.ReadAll(fpath))
}

func Read(lk sync.T) (r []*dailyChart.T) {
	for _, e := range ReadJs(lk).Ra() {
		r = append(r, dailyChart.FromJs(e))
	}
	return
}

func Write(lk sync.T, data []*dailyChart.T) {
	var a []json.T
	for _, e := range data {
		a = append(a, e.ToJs())
	}
	file.WriteAll(fpath, json.Wa(a).String())
}
