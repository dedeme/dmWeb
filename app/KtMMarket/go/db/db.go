// Copyright 01-Jul-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Data base management
package db

import (
	"github.com/dedeme/KtMMarket/cts"
	"github.com/dedeme/KtMMarket/data/model"
	"github.com/dedeme/KtMMarket/data/modelEval"
	"github.com/dedeme/KtMMarket/data/quotes"
	"github.com/dedeme/KtMMarket/data/simProfits"
	"github.com/dedeme/KtMMarket/db/quotesReader"
	"github.com/dedeme/KtMMarket/fns"
	"github.com/dedeme/ktlib/file"
	"github.com/dedeme/ktlib/jstb"
	"github.com/dedeme/ktlib/path"
)

func Initialize() {
	if !file.Exists(cts.KtMMarketDataDir) {
		file.Mkdir(cts.KtMMarketDataDir)
		file.Mkdir(path.Cat(cts.KtMMarketDataDir, "evals"))
		file.Mkdir(path.Cat(cts.KtMMarketDataDir, "simulation"))
		file.Write(path.Cat(cts.KtMMarketDataDir, "version.txt"), cts.DataVersion)
	}

	dataVersion := file.Read(path.Cat(cts.KtMMarketDataDir, "version.txt"))
	if dataVersion != cts.DataVersion {
		panic(
			"Application can not continue.\n" +
				"Expected data version:\n" +
				cts.DataVersion +
				"\nBut found:\n" +
				dataVersion)
	}

	quotesPath := path.Cat(cts.KtMMarketDataDir, "quotes.tb")
	if !file.Exists(quotesPath) {
		file.Write(quotesPath, quotes.ToJs(quotesReader.Read()))
	}
	for _, md := range model.List() {
		modelEvalsPath := path.Cat(cts.KtMMarketDataDir, "evals", md.Id+".tb")
		simProfitsPath := path.Cat(cts.KtMMarketDataDir, "simulation", md.Id+".tb")
		if !file.Exists(modelEvalsPath) || !file.Exists(simProfitsPath) {
			evals, profits := md.RangeNewSimulation(
				QuotesTb().Read(),
				[]*modelEval.T{},
				[]*simProfits.RowT{},
			)
			file.Write(modelEvalsPath, modelEval.TbToJs(modelEval.NewTb(
				fns.LastSunday(), evals,
			)))
			file.Write(simProfitsPath, simProfits.TbToJs(simProfits.NewTb(
				fns.LastSunday(), profits,
			)))
		}
	}
}

// Returns 'quotes.tb'
func QuotesTb() *jstb.T[*quotes.T] {
	return jstb.New(
		path.Cat(cts.KtMMarketDataDir, "quotes.tb"),
		nil,
		quotes.ToJs,
		quotes.FromJs,
	)
}

// Returns 'evals/' + modelId + '.tb'
func EvalsDb(modelId string) *jstb.T[*modelEval.TbT] {
	return jstb.New(
		path.Cat(cts.KtMMarketDataDir, "evals", modelId+".tb"),
		nil,
		modelEval.TbToJs,
		modelEval.TbFromJs,
	)
}

// Returns 'simulation/' + modelId + '.tb'
func SimProfitsDb(modelId string) *jstb.T[*simProfits.TbT] {
	return jstb.New(
		path.Cat(cts.KtMMarketDataDir, "simulation", modelId+".tb"),
		nil,
		simProfits.TbToJs,
		simProfits.TbFromJs,
	)
}
