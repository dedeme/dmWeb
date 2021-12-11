// Copyright 04-Jul-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Calculus of results of flea models with only one parameter.
package fleaResults

import (
	"github.com/dedeme/MultiMarket/data/cts"
	"github.com/dedeme/MultiMarket/data/flea"
	"github.com/dedeme/MultiMarket/data/flea/eFlea"
	"github.com/dedeme/MultiMarket/data/flea/fmodel"
	"github.com/dedeme/MultiMarket/data/flea/fmodels"
	"github.com/dedeme/MultiMarket/data/flea/frank"
	"github.com/dedeme/MultiMarket/data/flea/investor"
	"github.com/dedeme/MultiMarket/data/flea/jumpRanking"
	"github.com/dedeme/MultiMarket/data/flea/jumpResult"
	"github.com/dedeme/MultiMarket/data/flea/paramEval"
	"github.com/dedeme/MultiMarket/data/flea/result"
	"github.com/dedeme/MultiMarket/data/qtable"
	"github.com/dedeme/MultiMarket/db/fleas/fmodelsDb"
	"github.com/dedeme/MultiMarket/db/fleas/resultsDb"
	"github.com/dedeme/MultiMarket/db/quotesDb"
	"github.com/dedeme/MultiMarket/global/fn"
	"github.com/dedeme/MultiMarket/global/sync"
	"github.com/dedeme/golib/date"
	"github.com/dedeme/golib/file"
	"strings"
)

func evaluate(md *fmodel.T, f *flea.T, opens, closes *qtable.T) (
	value, sales float64,
) {
	rs := md.Assets(opens, closes, f.Param())
	sales = float64(rs.Sells())
	avg, _ := md.ProfitsAvgSd(opens, closes, f.Param())
	value = f.Evaluate(rs.Assets(), avg)
	return
}

func updateModelRanges(lk sync.T, md *fmodel.T, opens, closes *qtable.T) {
	now := date.Now().String()

	mkEflea := func(param float64, value float64, sales float64) *eFlea.T {
		eflea := eFlea.New(flea.New(param))
		eFlea.Evaluate(md, opens, closes, []*eFlea.T{eflea})
		eflea.HistoricSales = sales
		eflea.HistoricEval = value
		return eflea
	}
	ranking := paramEval.Ranking(resultsDb.ReadResults(lk, md.Id()), mkEflea)

	var ranks []*frank.T
	olds := fmodelsDb.Read(lk, md.Id())
	if len(olds) == 0 || olds[0].Date() < now {
		ranks = append([]*frank.T{frank.New(now, ranking)}, olds...)
	} else {
		olds[0] = frank.New(now, ranking)
		ranks = olds
	}

	fmodelsDb.Write(lk, md.Id(), ranks)
}

func calculateModel(lk sync.T, md *fmodel.T, opens, closes *qtable.T) {
	days, dt := resultsDb.ReadDaysDateModel(lk, md.Id())

	now := date.Now().String()
	tmpFile := resultsDb.OpenTmp(lk)
	if days == 0 {

		param := cts.RangesMin
		for {
			if param >= cts.RangesMax {
				break
			}

			flea := flea.New(param)
			value, sales := evaluate(md, flea, opens, closes)

			file.WriteBin(tmpFile, result.ToBits(param, value, sales, value, sales))

			param += 0.000001
		}

		days = 1

	} else {

		fun := func(
			param, value, sales, lastValue, lastSales float64,
		) bool {
			if param < cts.RangesMin || param >= cts.RangesMax {
				return false
			}

			fl := flea.New(param)
			newValue, newSales := evaluate(md, fl, opens, closes)

			if dt == now {
				value += (newValue - lastValue) / float64(days)
				sales += (newSales - lastSales) / float64(days)
			} else {
				days1 := days + 1
				value = (value*float64(days) + newValue) / float64(days1)
				sales = (sales*float64(days) + newSales) / float64(days1)
			}

			file.WriteBin(
				tmpFile,
				result.ToBits(param, value, sales, newValue, newSales),
			)

			return false
		}

		resultsDb.EachResult(lk, md.Id(), fun)

		if dt != now && days < cts.RangesAvg {
			days++
		}

	}

	tmpFile.Close()

	resultsDb.UpdateResults(lk, md.Id())
	resultsDb.WriteDaysDate(lk, md.Id(), days, now)

	updateModelRanges(lk, md, opens, closes)
}

// Update the ranking of jumps to operate.
func calculateJumps(lk sync.T, modelIds []string, opens, closes *qtable.T) {
	var maxParams []*paramEval.T

	resultsDb.EachResults(lk, modelIds, func(param, eval, sales float64) bool {
		var tmpMaxParams []*paramEval.T
		i := 0
		notAdded := true
		for _, pe := range maxParams {
			if i >= cts.JumpsMaxRanking {
				break
			}
			if notAdded {
				if eval > pe.Eval() {
					tmpMaxParams = append(tmpMaxParams, paramEval.New(param, eval, sales))
					notAdded = false
					i++
					if i >= cts.JumpsMaxRanking {
						break
					}
				}
			}
			tmpMaxParams = append(tmpMaxParams, pe)
			i++
		}

		if i < cts.JumpsMaxRanking {
			maxParams = append(maxParams, paramEval.New(param, eval, sales))
		} else {
			maxParams = tmpMaxParams
		}

		return false
	})

	var jumpResults []*jumpResult.T
	for _, mp := range maxParams {
		param := mp.Param()
		var invs []*investor.T
		for _, id := range modelIds {
			md, _ := fmodels.GetModel(id)
			ef := eFlea.New(flea.New(param))
			eFlea.Evaluate(md, opens, closes, []*eFlea.T{ef})
			ef.HistoricEval = ef.Eval
			ef.HistoricSales = float64(ef.Sells())
			resultsDb.EachResult(lk, id, func(p, eval, sales, _, _ float64) bool {
				if fn.Eq(p, param, 0.0000001) {
					ef.HistoricEval = eval
					ef.HistoricSales = sales
					return true
				}
				return false
			})
			invs = append(invs, investor.New(md, ef))
		}

		jumpResults = append(jumpResults, jumpResult.New(
			mp.Param(),
			mp.Eval(),
			mp.Sales(),
			invs,
		))
	}

	resultsDb.WriteJumpRanks(lk, jumpRanking.Add(
		resultsDb.ReadJumpRanks(lk),
		jumpRanking.New(date.Now().String(), jumpResults),
	))
}

// Calculate and save results of flea models with only one parameter.
func Calculate() {
	var opens *qtable.T
	var closes *qtable.T
	sync.Run(func(lk sync.T) {
		var models []*fmodel.T
		var modelIds []string
		for _, md := range fmodels.List() {
			models = append(models, md)
			modelIds = append(modelIds, md.Id())
		}
		resultsDb.Clean(lk, modelIds)
		opens = quotesDb.Opens(lk)
		closes = quotesDb.Closes(lk)
		for _, md := range models {
			calculateModel(lk, md, opens, closes)
		}

		calculateJumps(lk, strings.Split(cts.Jumps, ","), opens, closes)
	})
}
