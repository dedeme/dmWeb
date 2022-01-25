// Copyright 08-Nov-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Global ranking page.
package ranking

import (
	"fmt"
	"github.com/dedeme/QMarket/data/cts"
	"github.com/dedeme/QMarket/data/model"
	"github.com/dedeme/QMarket/db/modelsDb"
	"github.com/dedeme/QMarket/db/ranksDb"
	"github.com/dedeme/QMarket/lock"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/json"
)

type rangeEntryT struct {
	paramId int
	value   float64
	sales   float64
}

func newRangeEntry(id int, value, sales float64) *rangeEntryT {
	return &rangeEntryT{id, value, sales}
}

func (re *rangeEntryT) toJs() json.T {
	return json.Wa([]json.T{
		json.Wi(re.paramId),
		json.Wd(re.value),
		json.Wd(re.sales),
	})
}

func mkRangesTable() json.T {
	var rss2 []*rangeEntryT
	fn := func(param int, rss []*model.RsT) bool {
		hv := 0.0
		hs := 0.0
		for qlevel := 0; qlevel < cts.Qlevels; qlevel++ {
			rs := rss[qlevel]
			hv += rs.HistoricValue()
			hs += rs.HistoricSales()
		}
		n := float64(cts.Qlevels)
		rss2 = append(rss2, newRangeEntry(param, hv/n, hs/n))
		return false
	}
	modelsDb.EachResult(fn)

	var rsGroups []*rangeEntryT
	lenGroups := cts.RangesGroupNumber / 10
	midLenGroups := lenGroups / 2
	for i := 1; i < cts.RangesGroups*10; i++ {
		param := rss2[i*lenGroups].paramId
		sumValue := 0.0
		sumSales := 0.0
		for j := 0; j < lenGroups; j++ {
			re := rss2[i*lenGroups+j-midLenGroups]
			sumValue += re.value
			sumSales += re.sales
		}
		n := float64(lenGroups)
		rsGroups = append(rsGroups, newRangeEntry(
			param, sumValue/n, sumSales/n,
		))
	}

	var tb []json.T
	for i := 0; i < len(rsGroups)-10; i++ {
		param := rsGroups[i+5].paramId
		sumValue := 0.0
		sumSales := 0.0
		for j := 0; j < 10; j++ {
			re := rsGroups[i+j]
			sumValue += re.value
			sumSales += re.sales
		}
		tb = append(tb, newRangeEntry(param, sumValue/10, sumSales/10).toJs())
	}

	return json.Wa(tb)
}

func Process(ck string, mrq map[string]json.T) string {
	rq := cgi.RqString(mrq, "rq")
	switch rq {
	case "idata":
		rp := map[string]json.T{}
		lock.Run(func() {
			rp["rangesTable"] = mkRangesTable()
			rp["avgTable"] = ranksDb.ReadAvg().ToJs()
			rp["table"] = ranksDb.ReadMix().ToJs()
		})
		return cgi.Rp(ck, rp)
	default:
		panic(fmt.Sprintf("Value of rq ('%v') is not valid", rq))
	}
}
