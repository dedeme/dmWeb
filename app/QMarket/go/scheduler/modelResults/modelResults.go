// Copyright 05-Nov-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Calculus of model results.
package modelResults

import (
	"fmt"
	"github.com/dedeme/QMarket/data/cts"
	"github.com/dedeme/QMarket/data/model"
	"github.com/dedeme/QMarket/data/qtable"
	"github.com/dedeme/QMarket/data/rank"
	"github.com/dedeme/QMarket/db/modelsDb"
	"github.com/dedeme/QMarket/db/quotesDb"
	"github.com/dedeme/QMarket/db/ranksDb"
	"github.com/dedeme/QMarket/lock"
	"github.com/dedeme/golib/date"
)

// Calculate and save results of models.
func Calculate() {
	var opens *qtable.T
	var closes *qtable.T
	lock.Run(func() {
		opens = quotesDb.Opens()
		closes = quotesDb.Closes()
	})

	now := date.Now().String()

	// Models --------------------------------------------------------------------

	max := cts.RangesMin + cts.RangesGroups
	for group := cts.RangesMin; group < max; group++ {
		var hist *model.TableT

		lock.Run(func() {
			var ok bool
			ok, hist = modelsDb.Read(group)
			if !ok {
				hist = model.NewEmptyTable()
			}
		})

		days := hist.Days()
		if days > cts.EvalAvgDays {
			days = cts.EvalAvgDays
		}
		start := group * cts.RangesGroupNumber
		end := start + cts.RangesGroupNumber
		if days == 0 {
			results := map[int][]*model.RsT{}
			for i := start; i < end; i++ {
				var rss []*model.RsT
				for ql := 0; ql < cts.Qlevels; ql++ {
					md := model.New(ql, i)
					md.FirstEvaluation(opens, closes)
					_, ev := md.Evaluation()
					_, hev := md.Hevaluation()
					rss = append(rss, model.NewRs(
						ev.Sales(), ev.Value(), hev.Sales(), hev.Value(),
					))
				}
				results[i] = rss
			}
			lock.Run(func() {
				modelsDb.Write(group, model.NewTable(now, 1, results))
			})
		} else if now > hist.Date() {
			results := map[int][]*model.RsT{}
			for i := start; i < end; i++ {
				if oldRs, ok := hist.Results()[i]; ok {
					var rss []*model.RsT
					for ql := 0; ql < cts.Qlevels; ql++ {
						md := model.New(ql, i)
						qlrs := oldRs[ql]
						md.AppendEvaluation(
							days, qlrs.HistoricSales(), qlrs.HistoricValue(),
							opens, closes,
						)
						_, ev := md.Evaluation()
						_, hev := md.Hevaluation()
						rss = append(rss, model.NewRs(
							ev.Sales(), ev.Value(), hev.Sales(), hev.Value(),
						))
					}
					results[i] = rss
				} else {
					panic(fmt.Sprintf("Historic values of model %v is missing", i))
				}
			}
			lock.Run(func() {
				modelsDb.Write(group, model.NewTable(now, days+1, results))
			})
		} else {
			results := map[int][]*model.RsT{}
			for i := start; i < end; i++ {
				if oldRs, ok := hist.Results()[i]; ok {
					var rss []*model.RsT
					for ql := 0; ql < cts.Qlevels; ql++ {
						md := model.New(ql, i)
						md.ModifyEvaluation(days, oldRs[ql], opens, closes)
						_, ev := md.Evaluation()
						_, hev := md.Hevaluation()
						rss = append(rss, model.NewRs(
							ev.Sales(), ev.Value(), hev.Sales(), hev.Value(),
						))
					}
					results[i] = rss
				} else {
					panic(fmt.Sprintf("Historic values of model %v is missing", i))
				}
			}
			lock.Run(func() {
				modelsDb.Write(group, model.NewTable(now, days, results))
			})
		}
	}

	// Rankings ------------------------------------------------------------------

	lock.Run(func() {
		var qrks []rank.T
		for qlevel := 0; qlevel < cts.Qlevels; qlevel++ {
			qrks = append(qrks, rank.New())
		}
		avgrk := rank.New()

		for gr := cts.RangesMin; gr < cts.RangesMin+cts.RangesGroups; gr++ {
			_, tb := modelsDb.Read(gr)
			for k, rs := range tb.Results() {
				sum := 0.0
				for qlevel := 0; qlevel < cts.Qlevels; qlevel++ {
					v := rs[qlevel].HistoricValue()
					sum += v
					qrks[qlevel].Add(model.New(qlevel, k), v)
				}
				avgrk.Add(model.New(0, k), sum/float64(cts.Qlevels))
			}
		}

		var qtbs []rank.TableT
		for qlevel, qrk := range qrks {
			rk := qrk.MkRank(qlevel, opens, closes, func(md *model.T) *model.RsT {
				_, t := modelsDb.Read(modelsDb.Group(md))
				return t.Results()[md.Id()][qlevel]
			})

			tb := ranksDb.Read(qlevel)
			tb.Add(rank.NewTableEntry(now, rk))
			ranksDb.Write(qlevel, tb)

			qtbs = append(qtbs, tb)
		}

		ranksDb.WriteMix(rank.MixTables(qtbs))

		avgTrk := avgrk.MkAvgRank(opens, closes, func(mdId int) []*model.RsT {
			_, t := modelsDb.Read(modelsDb.Group(model.New(0, mdId)))
			return t.Results()[mdId]
		})

		tbAvg := ranksDb.ReadAvg()
		tbAvg.Add(rank.NewAvgTableEntry(now, avgTrk))
		ranksDb.WriteAvg(tbAvg)
	})
}
