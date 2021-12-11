// Copyright 15-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// General ranking page.
package ranking

import (
	"fmt"
	"github.com/dedeme/MultiMarket/data/flea/fmodels"
	"github.com/dedeme/MultiMarket/data/flea/investor"
	"github.com/dedeme/MultiMarket/data/flea/irank"
	"github.com/dedeme/MultiMarket/db/fleas/fmodelsDb"
	"github.com/dedeme/MultiMarket/db/fleas/resultsDb"
	"github.com/dedeme/MultiMarket/global/sync"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/json"
	"sort"
)

type rankingSorter []*investor.T

func (a rankingSorter) Len() int      { return len(a) }
func (a rankingSorter) Swap(i, j int) { a[i], a[j] = a[j], a[i] }
func (a rankingSorter) Less(i, j int) bool {
	return a[i].Eflea().HistoricEval > a[j].Eflea().HistoricEval
}

func Process(ck string, mrq map[string]json.T) string {
	source := cgi.RqString(mrq, "source")
	switch source {
	case "ranking":
		rp := map[string]json.T{}
		sync.Run(func(lk sync.T) {
			var dates []string
			md0 := fmodels.List()[0]
			for _, frk := range fmodelsDb.Read(lk, md0.Id()) {
				dates = append(dates, frk.Date())
			}

			var iranks []*irank.T
			for _, dt := range dates {
				iranks = append(iranks, irank.New(dt, []*investor.T{}))
			}
			for _, md := range fmodels.List() {
				frks := fmodelsDb.Read(lk, md.Id())
				for i, irk := range iranks {
					i2 := i
					if i2 >= len(frks) {
						i2 = len(frks) - 1
					}
					newRank := irk.Ranking()
					for j, eflea := range frks[i2].Ranking() {
						if j == 15 {
							break
						}
						newRank = append(newRank, investor.New(md, eflea))
					}
					iranks[i] = irank.New(irk.Date(), newRank)
				}
			}

			for _, irk := range iranks {
				sort.Sort(rankingSorter(irk.Ranking()))
			}

			var rk []json.T
			for _, e := range iranks { // []*Irank.T
				rk = append(rk, e.ToJsClient())
			}
			rp["ranking"] = json.Wa(rk)
			rp["jranking"] = resultsDb.ReadJumpRanksJsClient(lk)
		})
		return cgi.Rp(ck, rp)
	default:
		panic(fmt.Sprintf("Value of source ('%v') is not valid", source))
	}
}
