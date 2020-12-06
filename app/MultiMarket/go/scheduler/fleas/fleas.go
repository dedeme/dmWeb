// Copyright 08-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Fleas selection process.
package fleas

import (
	"fmt"
	"github.com/dedeme/MultiMarket/data/cts"
	"github.com/dedeme/MultiMarket/data/flea"
	"github.com/dedeme/MultiMarket/data/flea/eval"
	"github.com/dedeme/MultiMarket/data/flea/evalDate"
	"github.com/dedeme/MultiMarket/data/flea/fmodel"
	fmodels "github.com/dedeme/MultiMarket/data/flea/fmodels"
	"github.com/dedeme/MultiMarket/data/flea/frank"
	"github.com/dedeme/MultiMarket/data/flea/investor"
	"github.com/dedeme/MultiMarket/data/flea/irank"
	"github.com/dedeme/MultiMarket/data/qtable"
	"github.com/dedeme/MultiMarket/db/fleas/flog"
	"github.com/dedeme/MultiMarket/db/fleas/fmodelsDb"
	"github.com/dedeme/MultiMarket/db/fleas/rankingTb"
	"github.com/dedeme/MultiMarket/db/log"
	"github.com/dedeme/MultiMarket/db/quotesDb"
	"github.com/dedeme/MultiMarket/global/fn"
	"github.com/dedeme/MultiMarket/global/sync"
	"github.com/dedeme/golib/date"
)

func report(efleas []*eval.T, logId string, cycle int) {
	sumAssets := 0.0
	sumProfitsAvg := 0.0
	sumProfitsSd := 0.0
	sumEval := 0.0
	first := efleas[0]
	minAssets := first.Assets()
	maxAssets := first.Assets()
	minProfitsAvg := first.ProfitsAvg()
	maxProfitsAvg := first.ProfitsAvg()
	minProfitsSd := first.ProfitsSd()
	maxProfitsSd := first.ProfitsSd()
	minEval := first.Eval
	maxEval := first.Eval

	for _, e := range efleas {
		assets := e.Assets()
		profitsAvg := e.ProfitsAvg()
		profitsSd := e.ProfitsSd()
		eval := e.Eval
		sumAssets += assets
		sumProfitsAvg += profitsAvg
		sumProfitsSd += profitsSd
		sumEval += eval
		if assets < minAssets {
			minAssets = assets
		}
		if assets > maxAssets {
			maxAssets = assets
		}
		if profitsAvg < minProfitsAvg {
			minProfitsAvg = profitsAvg
		}
		if profitsAvg > maxProfitsAvg {
			maxProfitsAvg = profitsAvg
		}
		if profitsSd < minProfitsSd {
			minProfitsSd = profitsSd
		}
		if profitsSd > maxProfitsSd {
			maxProfitsSd = profitsSd
		}
		if eval < minEval {
			minEval = eval
		}
		if eval > maxEval {
			maxEval = eval
		}
	}

	n := len(efleas)
	flog.Info(logId, fmt.Sprintf(
		"Cycle: %v. Survivers: %v"+
			"\n  Max = Assets: %v. "+
			"PrfAvg: %v. "+
			"PrfVa: %v. "+
			"Eval: %v. "+
			"\n  Avg = Assets: %v. "+
			"PrfAvg: %v. "+
			"PrfVa: %v. "+
			"Eval: %v. "+
			"\n  Min = Assets: %v. "+
			"PrfAvg: %v. "+
			"PrfVa: %v. "+
			"Eval: %v. ",
		cycle, n,
		fn.Fix(maxAssets, 2), fn.Fix(maxProfitsAvg, 4),
		fn.Fix(maxProfitsSd, 4), fn.Fix(maxEval*100, 2),
		fn.Fix(sumAssets/float64(n), 2), fn.Fix(sumProfitsAvg/float64(n), 4),
		fn.Fix(sumProfitsSd/float64(n), 4), fn.Fix(sumEval/float64(n)*100, 2),
		fn.Fix(minAssets, 2), fn.Fix(minProfitsAvg, 4),
		fn.Fix(minProfitsSd, 4), fn.Fix(minEval*100, 2),
	))

}

func runCycle(
	opens, closes *qtable.T, md *fmodel.T,
	minSells, maxSells int, efleas []*eval.T,
) []*eval.T {
	if len(efleas) < 25 {
		panic(fmt.Sprintf("Ev.Fleas %v (<25) in model %v", len(efleas), md.Id()))
	}

	var newEfleas []*eval.T
	mnSells := float64(minSells)
	mxSells := float64(maxSells)

	for {
		for _, e := range efleas {
			addEval := true
			if e.Eval < 0 {
				rs := md.Assets(opens, closes, e.Flea().Params())
				sells := rs.Sells()
				if sells >= int(mnSells) && sells <= int(mxSells) {
					avg, va := md.ProfitsAvgSd(opens, closes, e.Flea().Params())
					e.Update(rs.Buys(), sells, rs.Assets(), avg, va)
				} else {
					addEval = false
				}
			}
			if addEval {
				newEfleas = append(newEfleas, e)
			}
		}

		if len(newEfleas) >= 20 {
			break
		}

		mxSells *= 1.1
		mnSells /= 1.1
		sync.Run(func(lk sync.T) {
			log.Error(lk, "Less than 20 surviver fleas in model "+md.Id())
		})
	}

	sum := 0.0
	for _, e := range newEfleas {
		ev := e.Flea().Evaluate(
			e.Assets()/cts.InitialCapital,
			e.ProfitsAvg()+1,
			e.ProfitsSd(),
		)
		e.Eval = ev
		sum += ev
	}
	avg := sum / float64(len(newEfleas))

	var tmp []*eval.T
	sum = 0
	for _, e := range newEfleas {
		if e.Eval >= avg {
			tmp = append(tmp, e)
			sum += e.Eval
		}
	}
	avg = sum / float64(len(tmp))

	var r []*eval.T
	for _, e := range tmp {
		if e.Eval >= avg {
			r = append(r, e)
		}
	}

	return r
}

func Evolution() {
	dt := date.Now().String()
	var rankingPoolNews []*investor.T
	var opens *qtable.T
	var closes *qtable.T
	sync.Run(func(lk sync.T) {
		opens = quotesDb.Opens(lk)
		closes = quotesDb.Closes(lk)
	})

	// Model rankings ----------------------------------------

	for _, md := range fmodels.List() {
		efleas := Selection(opens, closes, md, "")
		eval.Sort(efleas)

		bests := efleas
		if len(bests) > cts.PoolAddNumber {
			bests = bests[:cts.PoolAddNumber]
		}
		for _, e := range bests {
			rankingPoolNews = append(rankingPoolNews, investor.New(md, e))
		}

		sync.Run(func(lk sync.T) {
			pool := fmodelsDb.ReadPool(lk, md.Id())
			pool = eval.Complete(efleas, pool, cts.PoolNumber+cts.PoolAddNumber)
			eval.Evaluate(md, opens, closes, pool)
			eval.Sort(pool)
			if len(pool) > cts.PoolNumber {
				pool = pool[:cts.PoolNumber]
			}
			fmodelsDb.WritePool(lk, md.Id(), pool)

			bests := fmodelsDb.ReadBests(lk, md.Id())
			if len(bests) > 0 && bests[0].Date() == dt {
				bests = bests[1:]
			}
			bests = append([]*evalDate.T{evalDate.New(dt, pool[0])}, bests...)
			if len(bests) > cts.PoolNumber {
				bests = bests[:cts.PoolNumber]
			}
			fmodelsDb.WriteBests(lk, md.Id(), bests)

			ranks := fmodelsDb.ReadRanking(lk, md.Id())
			if len(ranks) > 0 && ranks[0].Date() == dt {
				ranks = ranks[1:]
			}
			var newRank *frank.T
			if len(ranks) == 0 {
				pool2 := pool
				if len(pool2) > cts.RankingNumber {
					pool2 = pool2[:cts.RankingNumber]
				}
				newRank = frank.New(dt, pool2)
			} else {
				n := cts.RankingNumber - cts.RankingChanges
				rank := make([]*eval.T, len(ranks[0].Ranking()))
				copy(rank, ranks[0].Ranking())
				if len(rank) > n {
					rank = rank[:n]
				}
				efs := eval.Complete(pool, rank, cts.RankingNumber)
				eval.Evaluate(md, opens, closes, efs)
				eval.Sort(efs)
				newRank = frank.New(dt, efs)
			}
			ranks = append([]*frank.T{newRank}, ranks...)
			if len(ranks) > cts.RankingDays {
				ranks = ranks[:cts.RankingDays]
			}
			fmodelsDb.WriteRanking(lk, md.Id(), ranks)
		})
	}

	// Global ranking ----------------------------------------

	// Auxiliar type
	type modelPoolT struct {
		md   *fmodel.T
		pool []*eval.T
	}

	sync.Run(func(lk sync.T) {
		var mdps []*modelPoolT
		for _, md := range fmodels.List() {
			mdps = append(mdps, &modelPoolT{md, fmodelsDb.ReadPool(lk, md.Id())})
		}
		n := cts.RankingNumber - len(mdps)
		ranks := rankingTb.Read(lk)
		if len(ranks) > 0 && ranks[0].Date() == dt {
			ranks = ranks[1:]
		}
		var lastRank []*investor.T
		if len(ranks) > 0 {
			lastRank = ranks[0].Ranking()
		}

		var invs []*investor.T
		for i, e := range lastRank {
			if i < n {
				invs = append(invs, e)
			} else if i < cts.RankingNumber {
				mdp := mdps[0]
				mdps = mdps[1:]
				md := mdp.md
				pool := mdp.pool
				ix := 0
				var inv *investor.T
				for {
					inv = investor.New(md, pool[ix])
					ix++
					if ix >= len(pool) || !inv.IsIn(invs) {
						break
					}
				}
				invs = append(invs, inv)
			}
		}

		if len(invs) < cts.RankingNumber {
			for _, md := range fmodels.List() {
				mdps = append(mdps, &modelPoolT{md, fmodelsDb.ReadPool(lk, md.Id())})
			}
			imdps := 0
			ix := 0
			for len(invs) < cts.RankingNumber {
				mdp := mdps[imdps]
				imdps++
				pool := mdp.pool
				if ix < len(pool) {
					inv := investor.New(mdp.md, pool[ix])
					if !inv.IsIn(invs) {
						invs = append(invs, inv)
					}
				}
				if imdps >= len(mdps) {
					imdps = 0
					ix++
				}
			}
		}

		investor.Evaluate(opens, closes, invs)
		investor.Sort(invs)
		ranks = append([]*irank.T{irank.New(dt, invs)}, ranks...)
		if len(ranks) > cts.RankingDays {
			ranks = ranks[:cts.RankingDays]
		}
		rankingTb.Write(lk, ranks)
	})
}

// Returns selected 'evaluated fleas'.
//    Opens : Open quotes table.
//    Closes: Close quotes table.
//    md    : Flea model.
//    logId : Identifier to activate log reports. If its value is "" no report
//            will be emitied.
func Selection(
	opens, closes *qtable.T, md *fmodel.T, logId string,
) []*eval.T {
	var efleas []*eval.T
	dt := date.Now().String()
	var pool []*eval.T
	sync.Run(func(lk sync.T) {
		pool = fmodelsDb.ReadPool(lk, md.Id())
		for _, e := range fmodelsDb.ReadBests(lk, md.Id()) {
			f := e.Eflea()
			if !f.IsIn(pool) {
				pool = append(pool, f)
			}
		}
	})

	minSells := int(fn.Fix(cts.HistoricQuotes*cts.MinSells, 0))
	maxSells := int(fn.Fix(cts.HistoricQuotes*cts.MaxSells, 0))
	nParams := len(md.ParMins())
	nCycles := cts.InsertionCycle + cts.Cycles*nParams + 1
	for cycle := 0; cycle < nCycles; cycle++ {
		if cycle == 0 {
			for i := 0; i < cts.FleasPerModel; i++ {
				efleas = append(efleas, eval.New(flea.New(md, dt, cycle, i)))
			}
		} else if cycle == cts.InsertionCycle {
			efleas = append(efleas, pool...)
			for i := len(efleas); i < cts.FleasPerModel; i++ {
				efleas = append(efleas, eval.New(flea.New(md, dt, cycle, i)))
			}
		} else {
			olds := efleas
			lolds := len(olds)
			id := 0
			i := 0
			for len(efleas) <= cts.FleasPerModel {
				if i >= lolds {
					i = 0
				}
				of := olds[i]
				i++
				efleas = append(efleas, eval.New(of.Flea().Mutate(md, dt, cycle, id)))
				id++
				efleas = append(efleas, eval.New(flea.New(md, dt, cycle, id)))
				id++
				efleas = append(efleas, eval.New(flea.New(md, dt, cycle, id)))
				id++
			}
		}

		efleas = runCycle(opens, closes, md, minSells, maxSells, efleas)

		if logId != "" {
			report(efleas, logId, cycle)
		}
	}

	if logId != "" {
		flog.Stop(logId)
	}

	return efleas
}
