// Copyright 19-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Accounting-trading page.
package trading

import (
	"fmt"
	"github.com/dedeme/MultiMarket/data/acc"
	"github.com/dedeme/MultiMarket/data/cts"
	"github.com/dedeme/MultiMarket/db/acc/diariesDb"
	"github.com/dedeme/MultiMarket/db/conf"
	"github.com/dedeme/MultiMarket/db/dailyTb"
	"github.com/dedeme/MultiMarket/db/managersTb"
	"github.com/dedeme/MultiMarket/db/nicksTb"
	"github.com/dedeme/MultiMarket/db/quotesDb"
	"github.com/dedeme/MultiMarket/global/sync"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/json"
)

type operationT struct {
	toBuy    bool
	toSell   int
	managers []int
	nick     string
}

func newOperation(
	toBuy bool, toSell int, managers []int, nick string,
) *operationT {
	return &operationT{toBuy, toSell, managers, nick}
}

func (o *operationT) toJs() json.T {
	var managers []json.T
	for _, e := range o.managers {
		managers = append(managers, json.Wi(e))
	}
	return json.Wa([]json.T{
		json.Wb(o.toBuy),
		json.Wi(o.toSell),
		json.Wa(managers),
		json.Ws(o.nick),
	})
}

func addBuy(ops []*operationT, manager int, nick string) (r []*operationT) {
	new := true
	for _, e := range ops {
		if e.nick == nick && e.toBuy {
			e.managers = append(e.managers, manager)
			r = append(r, e)
			new = false
		}
	}
	if new {
		r = append(r, newOperation(true, 0, []int{manager}, nick))
	}
	return
}

func addSell(
	ops []*operationT, stocks int, manager int, nick string,
) (r []*operationT) {
	new := true
	for _, e := range ops {
		if e.nick == nick && e.toSell > 0 {
			e.toSell += stocks
			e.managers = append(e.managers, manager)
			r = append(r, e)
			new = false
		}
	}
	if new {
		r = append(r, newOperation(false, stocks, []int{manager}, nick))
	}
	return
}

func Process(ck string, mrq map[string]json.T) string {
	rq := cgi.RqString(mrq, "rq")
	switch rq {
	case "idata":
		rp := map[string]json.T{}
		sync.Run(func(lk sync.T) {
			activity := conf.Activity(lk).Activity()
			closes := quotesDb.Closes(lk)
			nicks := closes.Nicks()
			values := closes.Values()
			lastV := len(values) - 1
			lastV1 := lastV - 1
			nkQ := map[string]float64{}
			nkQ1 := map[string]float64{}
			for i, e := range nicks {
				nkQ[e] = values[lastV][i]
				nkQ1[e] = values[lastV1][i]
			}

			mans := managersTb.Read(lk)
			qs := dailyTb.Read(lk)
			mqs := map[string]float64{}
			for _, q := range qs {
				nk, ok := nicksTb.GetNick(lk, q.Nick)
				if ok {
					mqs[nk.Name()] = q.Value
				}
			}

			var operations []*operationT
			for i, man := range mans {
				anns := diariesDb.ReadAnnotations(lk, i)
				_, portfolio, _ := acc.Settlement(anns)
				for _, nk := range nicks {
					q1, okQ1 := nkQ1[nk]
					q, okQ := nkQ[nk]
					ref1 := -1.0
					ref := -1.0
					if activity == cts.ActSleeping1 || activity == cts.ActHistoric {
						qnew, ok := mqs[nk]
						if ok {
							q1 = q
							q = qnew
							entry := man.GetModel(nk)
							cs, ok := closes.NickValuesAdd(nk, q)
							if ok {
								refs := entry.Model().Refs(cs, entry.Params())
								ref1 = refs[len(refs)-2]
								ref = refs[len(refs)-1]
							}
						}
					} else {
						entry := man.GetModel(nk)
						cs, ok := closes.NickValues(nk)
						if ok {
							refs := entry.Model().Refs(cs, entry.Params())
							ref1 = refs[len(refs)-2]
							ref = refs[len(refs)-1]
						}
					}

					if ref1 >= 0 && ref >= 0 && okQ1 && okQ {
						if ref1 > q1 && ref < q { // buy
							operations = addBuy(operations, i, nk)
						} else if ref1 < q1 && ref > q { // sell
							var pfe *acc.PfEntryT
							for _, e := range portfolio {
								if e.Nick() == nk {
									pfe = e
									break
								}
							}
							if pfe != nil {
								operations = addSell(operations, pfe.Stocks(), i, nk)
							}
						}
					}
				}
			}

			var ops []json.T
			for _, e := range operations {
				ops = append(ops, e.toJs())
			}

			rp["bet"] = json.Wd(cts.Bet)
			rp["operations"] = json.Wa(ops)
		})
		return cgi.Rp(ck, rp)
	default:
		panic(fmt.Sprintf("Value of rq ('%v') is not valid", rq))
	}
}
