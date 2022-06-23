// Copyright 28-Jun-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// StocksPage javascript page
package stocks

import (
	"github.com/dedeme/Hconta/data/broker"
	"github.com/dedeme/golib/date"
	"github.com/dedeme/golib/file"
	"github.com/dedeme/golib/json"
	"math"
	"path"
	"strconv"
)

const dKtMarket = "/home/deme/.dmGoApp/KtMarket/data/acc"

// Each element in entries is: nick, stocks, price
func ktMarketData(lastDate string) (entries []json.T, cash float64) {
	if !file.Exists(dKtMarket) {
		panic(dKtMarket + " not found")
	}

	year := date.Now().Format("%Y") + ".tb"
	var portfolios []map[string][]json.T
	for i := 0; i < investors; /*cts.go*/ i++ {
		pf := map[string][]json.T{}
		portfolios = append(portfolios, pf)
		fdb := path.Join(
			dKtMarket, "Investor-"+strconv.Itoa(i), "diaries", year)

		for _, es := range json.FromString(file.ReadAll(fdb)).Ra()[1].Ra() {
			ejs := es.Ra()
      if ejs[1].Rs() > lastDate {
        continue
      }
			tp := ejs[2].Rs()
			switch tp {
			case "st":
				nick := ejs[3].Rs()
				stks := ejs[4].Ri()
				cost := ejs[5].Rd() * float64(stks)
				pf[nick] = []json.T{json.Wi(stks), json.Wd(cost)}
			case "bu":
				nick := ejs[3].Rs()
				eStks := ejs[4].Ri()
				price := ejs[5].Rd()
				eCost := price * float64(eStks)

				cash -= math.Round(broker.Buy(eStks, price)*100) / 100
				if oldV, ok := pf[nick]; ok {
					stks := eStks + oldV[0].Ri()
					cost := eCost + oldV[1].Rd()
					pf[nick] = []json.T{json.Wi(stks), json.Wd(cost)}
				} else {
					pf[nick] = []json.T{json.Wi(eStks), json.Wd(eCost)}
				}
			case "se":
				nick := ejs[3].Rs()
				eStks := ejs[4].Ri()
				price := ejs[5].Rd()

				cash += math.Round(broker.Sell(eStks, price)*100) / 100
				if oldV, ok := pf[nick]; ok {
					oldStocks := oldV[0].Ri()
					stks := oldStocks - eStks
					if stks <= 0 {
						delete(pf, nick)
					} else {
						oldCost := oldV[1].Rd()
						buyPrice := oldCost / float64(oldStocks)
						cost := oldCost - buyPrice*float64(eStks)
						pf[nick] = []json.T{json.Wi(stks), json.Wd(cost)}
					}
				}
			case "in", "pr", "pd":
				cash += ejs[3].Rd()
			case "wi", "fe", "nd":
				cash -= ejs[3].Rd()
			default:
				panic("Unknown operation")
			}
		}
	}

	portfolio := map[string][]json.T{}
	for i := 0; i < investors; /*cts.go*/ i++ {
		for k, vNew := range portfolios[i] {
			if vOld, ok := portfolio[k]; ok {
				stks := vOld[0].Ri() + vNew[0].Ri()
				cost := vOld[2].Rd() + vNew[1].Rd()
				price := math.Round(cost*10000/float64(stks)) / 10000
				portfolio[k] = []json.T{
					json.Wi(stks),
					json.Wd(price),
					json.Wd(cost),
				}
				continue
			}
			portfolio[k] = []json.T{
				vNew[0],
				json.Wd(math.Round(
					vNew[1].Rd()*10000/float64(vNew[0].Ri())) / 10000),
				vNew[1],
			}
		}
	}

	for k, v := range portfolio {
		entries = append(entries, json.Wa([]json.T{json.Ws(k), v[0], v[1]}))
	}

	return
}
