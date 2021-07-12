// Copyright 15-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// General ranges plus page.
package rangesPlus

import (
	"fmt"
	"github.com/dedeme/MultiMarket/data/flea/fmodels"
	"github.com/dedeme/MultiMarket/data/flea/investor"
	"github.com/dedeme/MultiMarket/data/flea/irank"
	"github.com/dedeme/MultiMarket/db/fleas/fmodelsDb"
	"github.com/dedeme/MultiMarket/global/sync"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/json"
	"sort"
)

type rankingSorter []*investor.T

func (a rankingSorter) Len() int      { return len(a) }
func (a rankingSorter) Swap(i, j int) { a[i], a[j] = a[j], a[i] }
func (a rankingSorter) Less(i, j int) bool {
	return a[i].Eflea().Eval > a[j].Eflea().Eval
}

func Process(ck string, mrq map[string]json.T) string {
	source := cgi.RqString(mrq, "source")
	switch source {
	case "rangesPlus":
		rp := map[string]json.T{}
		sync.Run(func(lk sync.T) {
			var dates []string
			md0 := fmodels.List()[0]
			for _, frk := range fmodelsDb.ReadRangesPlus(lk, md0.Id()) {
				dates = append(dates, frk.Date())
			}

			var iranks []*irank.T
			for _, dt := range dates {
				iranks = append(iranks, irank.New(dt, []*investor.T{}))
			}
			for _, md := range fmodels.List() {
				if len(md.ParNames()) != 1 {
					continue
				}

				frks := fmodelsDb.ReadRangesPlus(lk, md.Id())

				for i, irk := range iranks {
					newRank := irk.Ranking()
					for j, eflea := range frks[i].Ranking() {
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
		})
		return cgi.Rp(ck, rp)
	default:
		panic(fmt.Sprintf("Value of source ('%v') is not valid", source))
	}
}
