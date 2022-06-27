// Copyright 18-Jun-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package db

import (
	"github.com/dedeme/KtMarket/cts"
	"github.com/dedeme/KtMarket/data/acc"
	"github.com/dedeme/KtMarket/data/dailyChart"
	"github.com/dedeme/KtMarket/data/invOperation"
	"github.com/dedeme/KtMarket/data/investor"
	"github.com/dedeme/KtMarket/data/nick"
	"github.com/dedeme/KtMarket/data/quote"
	"github.com/dedeme/KtMarket/data/refBase"
	"github.com/dedeme/KtMarket/data/server"
	"github.com/dedeme/KtMarket/data/serverBox"
	"github.com/dedeme/KtMarket/data/strategy"
	"github.com/dedeme/KtMarket/db/acc/diariesDb"
	"github.com/dedeme/KtMarket/db/acc/profitsDb"
	"github.com/dedeme/ktlib/arr"
	"github.com/dedeme/ktlib/log"
	"github.com/dedeme/ktlib/math"
	"github.com/dedeme/ktlib/time"
)

// Returns dates of model company for before to after
func Dates() (dates []string, err string) {
	nksTb := NicksTb().Read()
	nk, ok := arr.Find(nksTb.List, func(nk *nick.T) bool {
		return nk.Id == nksTb.Model
	})
	if !ok {
		nk = nksTb.List[0]
	}

	qs, e := QsRead(nk.Name)
	if e != "" {
		err = e
		return
	}
	arr.ReverseIn(qs)
	dates = arr.Map(qs, func(q *quote.T) string {
		return q.Date
	})
	return
}

// Returns current closes of a company (from before to after), updated with
// the daily ones if these have a posterior data.
func CurrentCloses(nk *nick.T) (closes []float64, err string) {
	qs, e := QsRead(nk.Name)
	if e != "" {
		err = e
		return
	}
	lastQsDate := qs[0].Date
	arr.ReverseIn(qs)
	closes = arr.Map(qs, func(q *quote.T) float64 {
		return q.Close
	})

	dailyQs := DailyTb().Read()
	if dailyQs.Date > lastQsDate {
		c, ok := arr.Find(dailyQs.Values, func(idVal *nick.IdValT) bool {
			return idVal.Nick == nk.Id
		})
		if ok {
			closes = append(arr.Drop(closes, 1), c.Value)
		}
	}

	return
}

// Modify 'investors.tb' and 'refsBase.tb'
func UpdateInvestors() (err string) {
	dates, e := Dates()
	if e != "" {
		err = e
		return
	}
	nks := NicksTb().Read().List

	refsDb := RefBasesTb()
	refsTb := refsDb.Read()

	invsDb := InvestorsTb()
	invs := invsDb.Read().Investors
	if len(invs) > cts.Investors {
		invs = arr.Take(invs, cts.Investors)
	} else if len(invs) < cts.Investors {
		last := invs[len(invs)-1]
		for i := 0; i < cts.Investors-len(invs); i++ {
			invs = append(invs, last)
		}
	}

	var newRefs []*refBase.T
	var newInvs []*investor.T
	for _, inv := range invs {
		stBase := inv.Base
		oldNicks := inv.Nicks
		newNicks := map[string]*strategy.T{}
		for _, nk := range nks {
			id := nk.Name
			st, ok := oldNicks[id]
			if !ok {
				st = stBase
			}

			closes, e := CurrentCloses(nk)
			if e != "" {
				if err == "" {
					err = e
				} else {
					err = err + "\n" + e
				}

				if rfBase, ok := refsTb.Get(nk, stBase); ok {
					newRefs = append(newRefs, rfBase)
				}

				if !strategy.Eq(st, stBase) {
					if rfBase2, ok := refsTb.Get(nk, st); ok {
						newRefs = append(newRefs, rfBase2)
					}
				}

				newNicks[nk.Name] = st

				continue
			}

			rfBase, ok := refsTb.Get(nk, stBase)
			var newRfBase *refBase.T
			if ok {
				newRfBase = refBase.UpdateRefBase(rfBase, dates, closes)
			} else {
				newRfBase = refBase.MkRefBase(nk, stBase, dates, closes)
			}

			if !strategy.Eq(st, stBase) {
				rfBase2, ok := refsTb.Get(nk, st)
				var newRfBase2 *refBase.T
				if ok {
					newRfBase2 = refBase.UpdateRefBase(rfBase2, dates, closes)
				} else {
					newRfBase2 = refBase.MkRefBase(nk, st, dates, closes)
				}

				rf := newRfBase.Ref
				rf2 := newRfBase2.Ref
				cl := quote.LastValue(closes)

				if (rf > cl && rf2 > cl) || (rf < cl && rf2 < cl) {
					st = stBase
				} else {
					newRefs = append(newRefs, newRfBase2)
				}
			}

			newRefs = append(newRefs, newRfBase)
			newNicks[nk.Name] = st
		}
		newInvs = append(newInvs, investor.New(inv.Base, newNicks))
	}

	refsDb.Write(refBase.NewTb(newRefs))
	invsDb.Write(investor.NewTb(newInvs))
	return
}

// Calculate and save investors operations
func Operations() (err string) {
	nks := arr.Filter(NicksTb().Read().List, func(nk *nick.T) bool {
		return nk.IsSel
	})
	rebuyDate := time.ToStr(time.AddDays(time.Now(), -62))
	var rebuyNicks []string
	pfs := make([][]*acc.PfEntryT, cts.Investors)
	for i := 0; i < cts.Investors; i++ {
		anns := diariesDb.ReadAnnotations(i)
		_, pf, lastOs, es := acc.Settlement(anns)
		if len(es) > 0 {
			err = arr.Join(es, "\n")
			return
		}

		for k, v := range lastOs {
			if v.Date > rebuyDate {
				profs, ok := v.Profits()
				if ok && profs < cts.RebuyLimit {
					rebuyNicks = append(rebuyNicks, k)
				}
			}
		}
		pfs[i] = pf
	}

	var ops []*invOperation.T
	for _, nk := range nks {
		closes, e := CurrentCloses(nk)
		if e != "" {
			err = e
			return
		}
		closes = arr.Drop(closes, len(closes)-cts.ReferenceQuotes)
		lastClose, lastClose2 := quote.LastValue2(closes)

		invs := InvestorsTb().Read().Investors
		for i := 0; i < cts.Investors; i++ {
			inv := invs[i]

			st, ok := inv.Nicks[nk.Name]
			if !ok {
				st = inv.Base
			}
			initRef := -1.0
			refBase, ok := RefBasesTb().Read().Get(nk, st)
			if ok {
				initRef = refBase.Ref
			}
			refs := strategy.Refs(st, closes, initRef)
			lastRef, lastRef2 := quote.LastValue2(refs)

			if lastRef > lastClose {
				pfEntry, ok := arr.Find(pfs[i], func(e *acc.PfEntryT) bool {
					return e.Nick == nk.Name
				})
				if ok {
					ops = append(ops, invOperation.New(pfEntry.Stocks, i, nk.Name))
				}
			} else if lastRef < lastClose && lastRef2 > lastClose2 {
				stocks := 0
				if arr.Any(rebuyNicks, func(nkName string) bool {
					return nkName == nk.Name
				}) {
					stocks = -1
				}
				ops = append(ops, invOperation.New(stocks, i, nk.Name))
			}
		}
	}

	InvOperationsTb().Write(invOperation.NewTb(ops))
	return
}

func NextServer() {
	svs := arr.Filter(ServersTb().Read().List, func(sv *server.T) bool {
		if cf, ok := sv.DailyConf(); ok {
			return cf.Sel == cts.ServerActive || cf.Sel == cts.ServerSelected
		}
		return false
	})
	if len(svs) == 0 {
		panic("No daily server is defined")
	}

	sboxDb := ServerBoxTb()
	dsvs := arr.Drop(sboxDb.Read().Servers, 1)

	if len(dsvs) == 0 {
		dsvs = arr.Map(svs, func(sv *server.T) string {
			return sv.Name
		})
		arr.Shuffle(dsvs)
	}
	sboxDb.Write(serverBox.New(dsvs))
}

func ActivateDailyCharts() {
	pfs := make([][]*acc.PfEntryT, cts.Investors)
	for i := 0; i < cts.Investors; i++ {
		anns := diariesDb.ReadAnnotations(i)
		_, pf, _, serrs := acc.Settlement(anns)
		if len(serrs) != 0 {
			for _, e := range serrs {
				log.Error(e)
			}
			pf = []*acc.PfEntryT{}
		}
		pfs[i] = pf
	}

	selNicks := arr.Filter(NicksTb().Read().List, func(nk *nick.T) bool {
		return nk.IsSel
	})
	nks := arr.Map(selNicks, func(nk *nick.T) *dailyChart.T {
		close := 0.0
		hours := []int{0}
		quotes := []float64{0}
		invs := InvestorsTb().Read().Investors
		invsData := make([]*dailyChart.DataT, cts.Investors)
		for i := 0; i < cts.Investors; i++ {
			st := invs[i].Base
			invsData[i] = dailyChart.NewData(st.Model.Id, st.Params, 0.0, 0.0, 0.0)
		}

		r := dailyChart.New(nk.Name, close, hours, quotes, invsData)
		qs, err := QsRead(nk.Name)
		if err != "" {
			log.Error(err)
			return r
		}
		qs = arr.Take(qs, cts.ReferenceQuotes)
		arr.ReverseIn(qs)
		closes := arr.Map(qs, func(q *quote.T) float64 {
			return q.Close
		})
		close = quote.LastValue(closes)

		tm := time.Now()
		hours[0] = time.Hour(tm)
		quotes[0] = close

		for i := 0; i < cts.Investors; i++ {
			pfE, ok := arr.Find(pfs[i], func(e *acc.PfEntryT) bool {
				return e.Nick == nk.Name
			})
			if ok {
				invsData[i].Stocks = pfE.Stocks
				invsData[i].Price = pfE.Price
			}

			inv := invs[i]
			st, ok := inv.Nicks[nk.Name]
			if !ok {
				st = inv.Base
			}

			initRef := -1.0
			refBase, ok := arr.Find(RefBasesTb().Read().Refs, func(r *refBase.T) bool {
				return r.Nick.Name == nk.Name && r.Strategy == st
			})
			if ok {
				initRef = refBase.Ref
			}

			invsData[i].Ref = strategy.LastRef(st, closes, initRef)
		}

		return dailyChart.New(nk.Name, close, hours, quotes, invsData)
	})
	DailyChartTb().Write(dailyChart.NewTb(nks))
}

// readQs is the function 'net.ServerReadDaily'
func UpdateDailyCharts(readQs func(*server.T) ([]*nick.IdValT, []string, string)) {
	svName := ServerBoxTb().Read().Servers[0]
	svs := arr.Filter(ServersTb().Read().List, func(sv *server.T) bool {
		if cf, ok := sv.DailyConf(); ok {
			return cf.Sel == cts.ServerActive || cf.Sel == cts.ServerSelected
		}
		return false
	})
	sv, ok := arr.Find(svs, func(sv *server.T) bool {
		return sv.Name == svName
	})
	if !ok {
		log.Error("Daily server " + svName + " not found or not actived")
		return
	}

	qs, warns, err := readQs(sv)
	if err != "" {
		log.Error(err)
		return
	}
	if len(warns) > 0 {
		for _, w := range warns {
			log.Error(w)
		}
	}
	tm := time.ToStr(time.Now())
	dailyDb := DailyTb()
	dailyTb := dailyDb.Read()
	if tm == dailyTb.Date {
		eq := len(dailyTb.Values) != 0 // Set to false if there is no value.
		for _, nkVal := range dailyTb.Values {
			q, ok := arr.Find(qs, func(q *nick.IdValT) bool {
				return q.Nick == nkVal.Nick
			})
			if !ok || !math.Eq(q.Value, nkVal.Value, 0.0000001) {
				eq = false
				break
			}
		}
		if eq {
			return // if daylyTb is there are values and they are equals to new data
		}
	}
	dailyDb.Write(nick.NewTbIdVal(tm, qs))

	dailyChartDb := DailyChartTb()
	nks := NicksTb().Read().List
	nkDs := dailyChartDb.Read().Nicks
	hour := time.Hour(time.Now())
	for _, nkD := range nkDs {
		nk, ok := arr.Find(nks, func(nk *nick.T) bool {
			return nk.Name == nkD.Nick
		})
		if !ok {
			log.Error("Nick " + nkD.Nick + " not found")
			nkD.Hours = append(nkD.Hours, hour)
			nkD.Quotes = append(nkD.Quotes, nkD.Quotes[len(nkD.Quotes)-1])
			continue
		}

		q, ok := arr.Find(qs, func(q *nick.IdValT) bool {
			return q.Nick == nk.Id
		})
		if !ok {
			log.Error("Daily quote of " + nkD.Nick + " not found")
			nkD.Hours = append(nkD.Hours, hour)
			nkD.Quotes = append(nkD.Quotes, nkD.Quotes[len(nkD.Quotes)-1])
			continue
		}

		nkD.Hours = append(nkD.Hours, hour)
		nkD.Quotes = append(nkD.Quotes, q.Value)
	}
	dailyChartDb.Write(dailyChart.NewTb(nkDs))
}

func UpdateHistoricProfits() {
	invs := InvestorsTb().Read().Investors
	for i := 0; i < cts.Investors; i++ {
		inv := invs[i]

		anns := diariesDb.ReadAnnotations(i)
		ledger, portfolio, _, errs := acc.Settlement(anns)
		for _, e := range errs {
			log.Error(e)
		}
		base := ledger.Cash + ledger.Capital
		account := base + ledger.Stocks
		total := base
		risk := base
		for _, e := range portfolio {
			nk, ok := arr.Find(NicksTb().Read().List, func(n *nick.T) bool {
				return n.Name == e.Nick
			})
			if !ok {
				log.Error("Nick " + e.Nick + " not found")
				continue
			}
			closes, err := CurrentCloses(nk)
			if err != "" {
				log.Error(err)
				continue
			}
			closes = arr.Drop(closes, len(closes)-cts.ReferenceQuotes)
			lastClose := quote.LastValue(closes)

			st, ok := inv.Nicks[nk.Name]
			if !ok {
				st = inv.Base
			}
			initRef := -1.0
			refBase, ok := arr.Find(RefBasesTb().Read().Refs, func(r *refBase.T) bool {
				return r.Nick.Name == nk.Name && strategy.Eq(r.Strategy, st)
			})
			if ok {
				initRef = refBase.Ref
			}
			lastRef := strategy.LastRef(st, closes, initRef)
			if lastRef > lastClose { // Sell situation.
				lastRef = lastClose
			}

			total += float64(e.Stocks) * lastClose
			risk += float64(e.Stocks) * lastRef
		}
		profitsDb.Add(i, total, account, risk)
	}

}
