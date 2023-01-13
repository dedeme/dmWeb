// Copyright 28-Jun-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// StocksPage javascript page
package stocks

import (
	"github.com/dedeme/Hconta/data/broker"
	"github.com/dedeme/ktlib/time"
	"github.com/dedeme/ktlib/file"
	"github.com/dedeme/ktlib/js"
	"github.com/dedeme/ktlib/path"
	"github.com/dedeme/ktlib/str"
	"github.com/dedeme/ktlib/math"
)

const dKtMarket = "/home/deme/.dmGoApp/KtMarket/data/acc"

// Each element in entries is: nick, stocks, price
func ktMarketData(lastDate string) (entries []string, cash float64) {
	if !file.Exists(dKtMarket) {
		panic(dKtMarket + " not found")
	}

	year := time.Fmt("%Y", time.Now()) + ".tb"
	var portfolios []map[string][]string
	for i := 0; i < investors; /*cts.go*/ i++ {
		pf := map[string][]string{}
		portfolios = append(portfolios, pf)
		fdb := path.Cat(
			dKtMarket, str.Fmt("Investor-%v", i), "diaries", year)

		for _, es := range js.Ra(js.Ra(file.Read(fdb))[1]) {
			ejs := js.Ra(es)
      if js.Rs(ejs[1]) > lastDate {
        continue
      }
			tp := js.Rs(ejs[2])
			switch tp {
			case "st":
				nick := js.Rs(ejs[3])
				stks := js.Ri(ejs[4])
				cost := js.Rd(ejs[5]) * float64(stks)
				pf[nick] = []string{js.Wi(stks), js.Wd(cost)}
			case "bu":
				nick := js.Rs(ejs[3])
				eStks := js.Ri(ejs[4])
				price := js.Rd(ejs[5])
				eCost := price * float64(eStks)

				cash -= math.Round(broker.Buy(eStks, price), 2)
				if oldV, ok := pf[nick]; ok {
					stks := eStks + js.Ri(oldV[0])
					cost := eCost + js.Rd(oldV[1])
					pf[nick] = []string{js.Wi(stks), js.Wd(cost)}
				} else {
					pf[nick] = []string{js.Wi(eStks), js.Wd(eCost)}
				}
			case "se":
				nick := js.Rs(ejs[3])
				eStks := js.Ri(ejs[4])
				price := js.Rd(ejs[5])

				cash += math.Round(broker.Sell(eStks, price), 2)
				if oldV, ok := pf[nick]; ok {
					oldStocks := js.Ri(oldV[0])
					stks := oldStocks - eStks
					if stks <= 0 {
						delete(pf, nick)
					} else {
						oldCost := js.Rd(oldV[1])
						buyPrice := oldCost / float64(oldStocks)
						cost := oldCost - buyPrice*float64(eStks)
						pf[nick] = []string{js.Wi(stks), js.Wd(cost)}
					}
				}
			case "in", "pr", "pd":
				cash += js.Rd(ejs[3])
			case "wi", "fe", "nd":
				cash -= js.Rd(ejs[3])
			default:
				panic("Unknown operation")
			}
		}
	}

	portfolio := map[string][]string{}
	for i := 0; i < investors; /*cts.go*/ i++ {
		for k, vNew := range portfolios[i] {
			if vOld, ok := portfolio[k]; ok {
				stks := js.Ri(vOld[0]) + js.Ri(vNew[0])
				cost := js.Rd(vOld[2]) + js.Rd(vNew[1])
				price := math.Round(cost/float64(stks), 4)
				portfolio[k] = []string{
					js.Wi(stks),
					js.Wd(price),
					js.Wd(cost),
				}
				continue
			}
			portfolio[k] = []string{
				vNew[0],
				js.Wd(math.Round(
					js.Rd(vNew[1])/float64(js.Ri(vNew[0])), 4)),
				vNew[1],
			}
		}
	}

	for k, v := range portfolio {
		entries = append(entries, js.Wa([]string{js.Ws(k), v[0], v[1]}))
	}

	return
}
