// Copyright 14-Nov-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Accounting-trading page.
package trading

import (
	"fmt"
	"github.com/dedeme/QMarket/data/acc"
	"github.com/dedeme/QMarket/data/cts"
	"github.com/dedeme/QMarket/data/model"
	"github.com/dedeme/QMarket/db/acc/diariesDb"
	"github.com/dedeme/QMarket/db/confTb"
	"github.com/dedeme/QMarket/db/dailyDb/dailyTb"
	"github.com/dedeme/QMarket/db/investorsTb"
	"github.com/dedeme/QMarket/db/nicksTb"
	"github.com/dedeme/QMarket/db/quotesDb"
	"github.com/dedeme/QMarket/lock"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/json"
)

type operationT struct {
	toBuy     bool
	toSell    int
	investors []int
	nick      string
}

func newOperation(
	toBuy bool, toSell int, managers []int, nick string,
) *operationT {
	return &operationT{toBuy, toSell, managers, nick}
}

func (o *operationT) toJs() json.T {
	var investors []json.T
	for _, e := range o.investors {
		investors = append(investors, json.Wi(e))
	}
	return json.Wa([]json.T{
		json.Wb(o.toBuy),
		json.Wi(o.toSell),
		json.Wa(investors),
		json.Ws(o.nick),
	})
}

func addBuy(ops []*operationT, investor int, nick string) (r []*operationT) {
	r = ops
	new := true
	for _, e := range ops {
		if e.nick == nick && e.toBuy {
			e.investors = append(e.investors, investor)
			// r = append(r, e)
			new = false
		}
	}
	if new {
		r = append(r, newOperation(true, 0, []int{investor}, nick))
	}
	return
}

func addSell(
	ops []*operationT, stocks int, investor int, nick string,
) (r []*operationT) {
	r = ops
	new := true
	for _, e := range ops {
		if e.nick == nick && e.toSell > 0 {
			e.toSell += stocks
			e.investors = append(e.investors, investor)
			// r = append(r, e)
			new = false
		}
	}
	if new {
		r = append(r, newOperation(false, stocks, []int{investor}, nick))
	}
	return
}

func Process(ck string, mrq map[string]json.T) string {
	rq := cgi.RqString(mrq, "rq")
	switch rq {
	case "idata":
		rp := map[string]json.T{}
		lock.Run(func() {
			activity := confTb.Activity().Activity()
			closes := quotesDb.Closes()
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

			invsTb := investorsTb.Read()
			qs := dailyTb.Read()
			mqs := map[string]float64{}
			for _, q := range qs {
				nk, ok := nicksTb.Read().NickFromId(q.Nick())
				if ok {
					mqs[nk.Name()] = q.Value()
				}
			}

			var operations []*operationT
			for inv := 0; inv < cts.Qlevels; inv++ {
				anns := diariesDb.ReadAnnotations(inv)
				_, portfolio, _ := acc.Settlement(anns)
				for _, nk := range nicks {
					q1, okQ1 := nkQ1[nk]
					q, okQ := nkQ[nk]
					ref1 := -1.0
					ref := -1.0
					if activity == cts.ActSleeping1 ||
            activity == cts.ActSleeping3 ||
            activity == cts.ActHistoric {
						qnew, ok := mqs[nk]
						if ok {
							q1 = q
							q = qnew
							param := invsTb.Params[nk][inv]
							md := model.New(inv, param)
							cs, ok := closes.NickValuesAdd(nk, q)
							if ok {
								refs := md.Refs(cs)
								ref1 = refs[len(refs)-2]
								ref = refs[len(refs)-1]
							}
						}
					} else {
						param := invsTb.Params[nk][inv]
						md := model.New(inv, param)
						cs, ok := closes.NickValues(nk)
						if ok {
							refs := md.Refs(cs)
							ref1 = refs[len(refs)-2]
							ref = refs[len(refs)-1]
						}
					}

					if ref1 >= 0 && ref >= 0 && okQ1 && okQ {
						if ref1 > q1 && ref < q { // buy
							operations = addBuy(operations, inv, nk)
						} else if ref1 < q1 && ref > q { // sell
							var pfe *acc.PfEntryT
							for _, e := range portfolio {
								if e.Nick() == nk {
									pfe = e
									break
								}
							}
							if pfe != nil {
								operations = addSell(operations, pfe.Stocks(), inv, nk)
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
