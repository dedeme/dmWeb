// Copyright 12-Nov-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Daily charts data table.
package dailyChartsTb

import (
	"github.com/dedeme/QMarket/data/dailyChart"
	"github.com/dedeme/golib/file"
	"github.com/dedeme/golib/json"
	"path"
)

var fpath string

// Initializes charts data table.
//    parent: Parent directory.
func Initialize(parent string) {
	fpath = path.Join(parent, "DailyChart.tb")
	if !file.Exists(fpath) {
		Write([]*dailyChart.T{})
	}
}

func ReadJs() json.T {
	return json.FromString(file.ReadAll(fpath))
}

func Read() (r []*dailyChart.T) {
	for _, e := range ReadJs().Ra() {
		r = append(r, dailyChart.FromJs(e))
	}
	return
}

func Write(data []*dailyChart.T) {
	var a []json.T
	for _, e := range data {
		a = append(a, e.ToJs())
	}
	file.WriteAll(fpath, json.Wa(a).String())
}
