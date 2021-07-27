// Copyright 19-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Result speedometers page.
package speedometers

import (
	"fmt"
	"github.com/dedeme/MultiMarket/data/acc"
	"github.com/dedeme/MultiMarket/data/cts"
	"github.com/dedeme/MultiMarket/db/acc/diariesDb"
	"github.com/dedeme/MultiMarket/db/conf"
	"github.com/dedeme/MultiMarket/db/dailyTb"
	"github.com/dedeme/MultiMarket/db/investorsTb"
	"github.com/dedeme/MultiMarket/db/nicksTb"
	"github.com/dedeme/MultiMarket/db/quotesDb"
	"github.com/dedeme/MultiMarket/db/refsDb"
	"github.com/dedeme/MultiMarket/global/sync"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/date"
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

func accData(lk sync.T) (
	ledgers []*acc.LedgerT, // one per investor
	portfolios [][]*acc.PfEntryT, // one per investor
) {
	activity := conf.Activity(lk).Activity()
	dates := quotesDb.Dates(lk)
	dates2 := append(dates, date.Now().String())[1:]
	closes := quotesDb.Closes(lk)
	invs := investorsTb.Read(lk)
	qs := dailyTb.Read(lk)
	mqs := map[string]float64{}
	for _, q := range qs {
		nk, ok := nicksTb.GetNick(lk, q.Nick)
		if ok {
			mqs[nk.Name()] = q.Value
		}
	}

	for i, inv := range invs {
		anns := diariesDb.ReadAnnotations(lk, i)
		ledger, portfolio, _ := acc.Settlement(anns)
		ledgers = append(ledgers, ledger)
		var pf []*acc.PfEntryT
		for _, e := range portfolio {
			nk := e.Nick()
			quote := -1.0
			ref := e.Price()
			var dts = dates
			var cs [][]float64
			ok := false
			if activity != cts.ActSleeping2 {
				v, ok2 := mqs[nk]
				if ok2 {
					cs, ok = closes.NickValuesAdd(nk, v)
					dts = dates2
				} else {
					cs, ok = closes.NickValues(nk)
				}
			} else {
				cs, ok = closes.NickValues(nk)
			}
			if ok {
				mdPars := inv.GetModel(nk)
				quote = cs[len(cs)-1][0]
				refs := refsDb.MkRefs(
					lk, nk, dts, cs, mdPars.Model(), mdPars.Param(),
				)
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
		sync.Run(func(lk sync.T) {
			lds, pfs := accData(lk)
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
		sync.Run(func(lk sync.T) {
			_, pfs := accData(lk)
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
