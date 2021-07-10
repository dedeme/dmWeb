// Copyright 04-Jul-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Calculus of results of flea models with only one parameter.
package fleaResults

import (
	"github.com/dedeme/MultiMarket/data/cts"
	"github.com/dedeme/MultiMarket/data/flea"
	"github.com/dedeme/MultiMarket/data/flea/fmodel"
	"github.com/dedeme/MultiMarket/data/flea/fmodels"
	"github.com/dedeme/MultiMarket/data/flea/frank"
	"github.com/dedeme/MultiMarket/data/flea/paramEval"
	"github.com/dedeme/MultiMarket/data/flea/result"
	"github.com/dedeme/MultiMarket/data/qtable"
	"github.com/dedeme/MultiMarket/db/fleas/fmodelsDb"
	"github.com/dedeme/MultiMarket/db/fleas/resultsDb"
  "github.com/dedeme/MultiMarket/data/flea/eval"
	"github.com/dedeme/MultiMarket/db/log"
	"github.com/dedeme/MultiMarket/db/quotesDb"
	"github.com/dedeme/MultiMarket/global/sync"
	"github.com/dedeme/golib/date"
	"github.com/dedeme/golib/file"
)

func evaluate(md *fmodel.T, f *flea.T, opens, closes *qtable.T) (
	value float64, sales int,
) {
	rs := md.Assets(opens, closes, f.Params())
	sales = rs.Sells()
	avg, sd := md.ProfitsAvgSd(opens, closes, f.Params())
	value = f.Evaluate(rs.Assets()/cts.InitialCapital, avg+1, sd)
	return
}

func updateModelRangesPlus(lk sync.T, md *fmodel.T, opens, closes *qtable.T) {
	now := date.Now().String()

  mkEflea := func (param float64, value float64, sales int) *eval.T {
    eflea := eval.New(flea.NewWithParams(md, now, 0, 0, []float64{param}))
    eval.Evaluate(md, opens, closes, []*eval.T{eflea})
    eflea.Flea().ChangeName(md.Id(), value, sales)
    return eflea
  }
	ranking := paramEval.Ranking(resultsDb.ReadResults(lk, md.Id()), mkEflea)

	var ranks []*frank.T
  olds := fmodelsDb.ReadRangesPlus(lk, md.Id())
  if len(olds) == 0 || olds[0].Date() < now {
    ranks = append([]*frank.T{frank.New(now, ranking)}, olds...)
  } else {
    olds[0] = frank.New(now, ranking)
    ranks = olds
  }

	fmodelsDb.WriteRangesPlus(lk, md.Id(), ranks)
}

func calculateModel(lk sync.T, md *fmodel.T, opens, closes *qtable.T) {
	days, dt := resultsDb.ReadDaysDateModel(lk, md.Id())

	fail := ""
	now := date.Now().String()
	tmpFile := resultsDb.OpenTmp(lk)
	if days == 0 {

		param := cts.RangesMin
		for {
			if param >= cts.RangesMax {
				break
			}

			flea := flea.NewWithParams(md, now, 0, 0, []float64{param})
			value, sales := evaluate(md, flea, opens, closes)

			file.WriteBin(tmpFile, result.ToBits(param, value, sales, value, sales))

			param += 0.000001
		}

		days = 1

	} else {

		fn := func(
			param, value float64, sales int, lastValue float64, lastSales int,
		) bool {
			if param < cts.RangesMin || param >= cts.RangesMax {
				return false
			}

			flea := flea.NewWithParams(md, now, 0, 0, []float64{param})
			newValue, newSales := evaluate(md, flea, opens, closes)

			if dt == now {
				value += (newValue - lastValue) / float64(days)
				sales += (newSales - lastSales) / days
			} else {
				days1 := days + 1
				value = (value*float64(days) + newValue) / float64(days1)
				sales = (sales*days + newSales) / days1
			}

			file.WriteBin(
				tmpFile,
				result.ToBits(param, value, sales, newValue, newSales),
			)

			return false
		}

		resultsDb.EachResult(lk, md.Id(), fn)

		if dt != now && days < cts.RangesAvg {
			days++
		}

	}

	if fail != "" {
		log.Error(lk, fail)
		return
	}

	tmpFile.Close()

	resultsDb.UpdateResults(lk, md.Id())
	resultsDb.WriteDaysDate(lk, md.Id(), days, now)

	updateModelRangesPlus(lk, md, opens, closes)
}

// Calculate and save results of flea models with only one parameter.
func Calculate() {
		var opens *qtable.T
		var closes *qtable.T
		sync.Run(func(lk sync.T) {
			var models []*fmodel.T
			var modelIds []string
			for _, md := range fmodels.List() {
				if len(md.ParNames()) == 1 {
					models = append(models, md)
					modelIds = append(modelIds, md.Id())
				}
			}
			resultsDb.Clean(lk, modelIds)
			opens = quotesDb.Opens(lk)
			closes = quotesDb.Closes(lk)
			for _, md := range models {
				calculateModel(lk, md, opens, closes)
			}
		})
}
