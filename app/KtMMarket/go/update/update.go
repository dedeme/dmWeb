// Copyright 01-Jul-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Data base update.
package update

import (
	"github.com/dedeme/KtMMarket/data/model"
	"github.com/dedeme/KtMMarket/data/modelEval"
	"github.com/dedeme/KtMMarket/data/simProfits"
	"github.com/dedeme/KtMMarket/db"
	"github.com/dedeme/KtMMarket/db/quotesReader"
	"github.com/dedeme/KtMMarket/fns"
)

func Run() {
	lastSunday := fns.LastSunday()
	quotesDb := db.QuotesTb()
	qs := quotesDb.Read()
	if qs.Date < lastSunday {
		qs = quotesReader.Read()
		quotesDb.Write(qs)

		for _, md := range model.List() {
			evalsDb := db.EvalsDb(md.Id)
			evs := evalsDb.Read().Evals
			simProfitsDb := db.SimProfitsDb(md.Id)
			profRows := simProfitsDb.Read().Rows
			if qs.Date < lastSunday {
				evs, profRows = md.RangeNewSimulation(qs, evs, profRows)
			} else {
				evs, profRows = md.RangeReplaceSimulation(qs, evs, profRows)
			}
			evalsDb.Write(modelEval.NewTb(lastSunday, evs))
			simProfitsDb.Write(simProfits.NewTb(lastSunday, profRows))
		}
	}
}

// NOTE:
// md.RangeReplaceSimulation function is intended for special situations.
// They only will be called if the condition
//    'if qs.Date < fns.LastSunday() {'
// is deactivated.
