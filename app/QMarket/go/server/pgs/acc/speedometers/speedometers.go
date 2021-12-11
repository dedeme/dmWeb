// Copyright 19-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Result speedometers page.
package speedometers

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

type investorEntryT struct {
	currentProfits       float64
	riskProfits          float64
	active               float64
	stocksCurrentProfits float64
	stocksRiskProfits    float64
	stocksAccountValue   float64
}

func investorEntryNew(
	currentProfits,
	riskProfits,
	active,
	stocksCurrentProfits,
	stocksRiskProfits,
	stocksAccountValue float64,
) *investorEntryT {
	return &investorEntryT{
		currentProfits,
		riskProfits,
		active,
		stocksCurrentProfits,
		stocksRiskProfits,
		stocksAccountValue,
	}
}
func (i *investorEntryT) toJs() json.T {
	return json.Wa([]json.T{
		json.Wd(i.currentProfits),
		json.Wd(i.riskProfits),
		json.Wd(i.active),
		json.Wd(i.stocksCurrentProfits),
		json.Wd(i.stocksRiskProfits),
		json.Wd(i.stocksAccountValue),
	})
}

type dataT struct {
	isInvestor bool
	data       json.T
}

func dataNew(isInvestor bool, data json.T) *dataT {
	return &dataT{isInvestor, data}
}
func (d *dataT) toJs() json.T {
	return json.Wa([]json.T{
		json.Wb(d.isInvestor),
		d.data,
	})
}

// -----------------------------------------------------------------------------

func accData() (
	ledgers []*acc.LedgerT, // one per investor
	portfolios [][]*acc.PfEntryT, // one per investor
) {
	activity := confTb.Activity().Activity()
	closes := quotesDb.Closes()
	invsTb := investorsTb.Read()
	qs := dailyTb.Read()
	mqs := map[string]float64{}
	for _, q := range qs {
		nk, ok := nicksTb.Read().NickFromId(q.Nick())
		if ok {
			mqs[nk.Name()] = q.Value()
		}
	}

	for inv := 0; inv < cts.Qlevels; inv++ {
		anns := diariesDb.ReadAnnotations(inv)
		ledger, portfolio, _ := acc.Settlement(anns)
		ledgers = append(ledgers, ledger)
		var pf []*acc.PfEntryT
		for _, e := range portfolio {
			nk := e.Nick()
			quote := -1.0
			ref := e.Price()
			var cs [][]float64
			ok := false
			if activity != cts.ActSleeping2 {
				v, ok2 := mqs[nk]
				if ok2 {
					cs, ok = closes.NickValuesAdd(nk, v)
				} else {
					cs, ok = closes.NickValues(nk)
				}
			} else {
				cs, ok = closes.NickValues(nk)
			}
			if ok {
				param := invsTb.Params[nk][inv]
				md := model.New(inv, param)
				quote = cs[len(cs)-1][0]
				refs := md.Refs(cs)
				ref = refs[len(refs)-1]
				if ref > quote { // Sell situation.
					ref = quote
				}
			}
			pf = append(
				pf, acc.NewPfEntry(nk, e.Stocks(), e.Price(), quote, ref),
			)
		}
		portfolios = append(portfolios, pf)
	}

	return
}

func avg(
	oldStocks int, oldValue float64, newStocks int, newValue float64,
) float64 {
	return (float64(oldStocks)*oldValue + float64(newStocks)*newValue) /
		float64(oldStocks+newStocks)
}

// -----------------------------------------------------------------------------
func Process(ck string, mrq map[string]json.T) string {
	rq := cgi.RqString(mrq, "rq")
	switch rq {
	case "investorsData":
		rp := map[string]json.T{}
		lock.Run(func() {
			lds, pfs := accData()
			var invsData []json.T
			for i, pf := range pfs {
				currentValue := 0.0
				riskValue := 0.0
				for _, e := range pf {
					currentValue += float64(e.Stocks()) * e.Quote()
					riskValue += float64(e.Stocks()) * e.Ref()
				}

				accountValue := lds[i].Stocks()
				active := accountValue + lds[i].Cash()
				accountProfits := active + lds[i].Capital()
				stocksCurrentProfits := currentValue - accountValue
				stocksRiskProfits := riskValue - accountValue

				invsData = append(invsData, investorEntryNew(
					accountProfits+stocksCurrentProfits,
					accountProfits+stocksRiskProfits,
					active,
					stocksCurrentProfits,
					stocksRiskProfits,
					accountValue,
				).toJs())
			}

			rp["data"] = dataNew(true, json.Wa(invsData)).toJs()
		})
		return cgi.Rp(ck, rp)
	case "companiesData":
		rp := map[string]json.T{}
		lock.Run(func() {
			_, pfs := accData()
			mpPf := map[string]*acc.PfEntryT{}
			for _, pf := range pfs {
				for _, e := range pf {
					if e2, ok := mpPf[e.Nick()]; ok {
						mpPf[e.Nick()] = acc.NewPfEntry(
							e.Nick(),
							e.Stocks()+e2.Stocks(),
							avg(e2.Stocks(), e2.Price(), e.Stocks(), e.Price()),
							avg(e2.Stocks(), e2.Quote(), e.Stocks(), e.Quote()),
							avg(e2.Stocks(), e2.Ref(), e.Stocks(), e.Ref()),
						)
						continue
					}
					mpPf[e.Nick()] = e
				}
			}
			var ciasData []json.T
			for _, v := range mpPf {
				ciasData = append(ciasData, v.ToJs())
			}

			rp["data"] = dataNew(false, json.Wa(ciasData)).toJs()
		})
		return cgi.Rp(ck, rp)
	default:
		panic(fmt.Sprintf("Value of rq ('%v') is not valid", rq))
	}
}
