// Copyright 19-Jun-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// acc/speedometers page
package speedometers

import (
	"github.com/dedeme/KtMarket/cts"
	"github.com/dedeme/KtMarket/data/acc"
	"github.com/dedeme/KtMarket/data/nick"
	"github.com/dedeme/KtMarket/data/refBase"
	"github.com/dedeme/KtMarket/data/reference"
	"github.com/dedeme/KtMarket/data/strategy"
	"github.com/dedeme/KtMarket/db"
	"github.com/dedeme/KtMarket/db/acc/diariesDb"
	"github.com/dedeme/ktlib/arr"
	"github.com/dedeme/ktlib/cgi"
	"github.com/dedeme/ktlib/js"
	"github.com/dedeme/ktlib/log"
	"github.com/dedeme/ktlib/thread"
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
func (i *investorEntryT) toJs() string {
	return js.Wa([]string{
		js.Wd(i.currentProfits),
		js.Wd(i.riskProfits),
		js.Wd(i.active),
		js.Wd(i.stocksCurrentProfits),
		js.Wd(i.stocksRiskProfits),
		js.Wd(i.stocksAccountValue),
	})
}

type dataT struct {
	investors  int
	isInvestor bool
	data       string
}

func dataNew(investors int, isInvestor bool, data string) *dataT {
	return &dataT{investors, isInvestor, data}
}
func (d *dataT) toJs() string {
	return js.Wa([]string{
		js.Wi(d.investors),
		js.Wb(d.isInvestor),
		d.data,
	})
}

// -----------------------------------------------------------------------------

func accData() (
	ledgers []*acc.LedgerT, // one per investor
	portfolios [][]*acc.PfEntryT, // one per investor
) {
	for i := 0; i < cts.Investors; i++ {
		inv := db.InvestorsTb().Read().Investors[i]

		anns := diariesDb.ReadAnnotations(i)
		ledger, portfolio, _, errs := acc.Settlement(anns)
		for _, err := range errs {
			log.Error(err)
		}
		ledgers = append(ledgers, ledger)
		var pf []*acc.PfEntryT
		for _, e := range portfolio {

			nk, ok := arr.Find(db.NicksTb().Read().List, func(n *nick.T) bool {
				return n.Name == e.Nick
			})
			if !ok {
				log.Error("Nick " + e.Nick + " not found")
				continue
			}
			_, closes, err := db.CurrentCloses(nk)
			if err != "" {
				log.Error(err)
				continue
			}
			closes = arr.Drop(closes, len(closes)-cts.ReferenceQuotes)
			lastClose := closes[len(closes)-1]
			if lastClose < 0 {
				log.Error("Las close of " + nk.Name + " not found in 'utils.UpdateHistoricProfits'")
				continue
			}

			st, ok := inv.Nicks[nk.Name]
			if !ok {
				st = inv.Base
			}
			initRef := reference.New(-1.0, true)
			refBase, ok := arr.Find(db.RefBasesTb().Read().Refs, func(r *refBase.T) bool {
				return r.Nick.Name == nk.Name && strategy.Eq(r.Strategy, st)
			})
			if ok {
				initRef = refBase.Ref
			}
			lastRef := strategy.LastRef(st, closes, initRef).Ref
			if lastRef > lastClose { // Sell situation.
				lastRef = lastClose
			}

			pf = append(pf, acc.NewPfEntry(
				e.Nick, e.Stocks, e.Price, lastClose, lastRef,
			))
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

func Process(ck string, mrq cgi.T) string {
	rq := js.Rs(mrq["rq"])
	switch rq {
	case "investorsData":
		var data *dataT
		thread.Sync(func() {
			lds, pfs := accData()
			var invsData []string
			for i, pf := range pfs {
				currentValue := 0.0
				riskValue := 0.0
				for _, e := range pf {
					currentValue += float64(e.Stocks) * e.Quote
					riskValue += float64(e.Stocks) * e.Ref
				}

				accountValue := lds[i].Stocks
				active := accountValue + lds[i].Cash
				accountProfits := active + lds[i].Capital
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

			data = dataNew(cts.Investors, true, js.Wa(invsData))
		})
		return cgi.Rp(ck, cgi.T{
			"data": data.toJs(),
		})

	case "companiesData":
		var data *dataT
		thread.Sync(func() {
			_, pfs := accData()
			mpPf := map[string]*acc.PfEntryT{}
			for _, pf := range pfs {
				for _, e := range pf {
					if e2, ok := mpPf[e.Nick]; ok {
						mpPf[e.Nick] = acc.NewPfEntry(
							e.Nick,
							e.Stocks+e2.Stocks,
							avg(e2.Stocks, e2.Price, e.Stocks, e.Price),
							avg(e2.Stocks, e2.Quote, e.Stocks, e.Quote),
							avg(e2.Stocks, e2.Ref, e.Stocks, e.Ref),
						)
						continue
					}
					mpPf[e.Nick] = e
				}
			}
			var ciasData []string
			for _, v := range mpPf {
				ciasData = append(ciasData, acc.PfEntryToJs(v))
			}

			data = dataNew(cts.Investors, false, js.Wa(ciasData))
		})
		return cgi.Rp(ck, cgi.T{
			"data": data.toJs(),
		})

	default:
		panic("Value of rq (" + rq + ") is not valid")
	}
}
