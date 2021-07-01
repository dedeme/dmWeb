// Copyright 27-Jun-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// StocksPage javascript page
package stocks

import (
	"github.com/dedeme/golib/file"
	"github.com/dedeme/golib/json"
	"math"
)

const stocksPath = "/dm/wwwcgi/dmcgi/Stocks/data/all.tb"

// Each element in entries is: nick, stocks, price, cost
func stocksData() (entries []json.T, sum float64) {
	if !file.Exists(stocksPath) {
		panic(stocksPath + " not found")
	}
	yearsJs := json.FromString(file.ReadAll(stocksPath)).Ra()
	yearJs := yearsJs[len(yearsJs)-1].Ra()

	var portfolios []map[string][]json.T
	for i := 0; i < investors; /*cts.go*/ i++ {
		portfolios = append(portfolios, map[string][]json.T{})
	}

	for _, eJs := range yearJs[1].Ra()[1].Ra() {
		fields := eJs.Ra()
		isSale := fields[1].Rb()
		inv := fields[3].Ri()
		nick := fields[4].Rs()
		stks := fields[5].Ri()
		price := fields[6].Rd()
		cost := price * float64(stks)

		portfolio := portfolios[inv]
		if pfe, ok := portfolio[nick]; ok {
			if isSale {
				eStocks := pfe[0].Ri() - stks
				if eStocks == 0 {
					delete(portfolio, nick)
				} else {
					eCost := pfe[2].Rd() - float64(stks)*pfe[1].Rd()
					ePrice := math.Round(eCost*10000/float64(eStocks)) / 10000
					portfolio[nick] = []json.T{
						json.Wi(eStocks),
						json.Wd(ePrice),
						json.Wd(eCost),
					}
				}
			} else {
				eStocks := pfe[0].Ri() + stks
				if eStocks == 0 {
					delete(portfolio, nick)
				} else {
					eCost := pfe[2].Rd() + cost
					ePrice := math.Round(eCost*10000/float64(eStocks)) / 10000
					portfolio[nick] = []json.T{
						json.Wi(eStocks),
						json.Wd(ePrice),
						json.Wd(eCost),
					}
				}
			}
		} else {
			if isSale {
				portfolio[nick] = []json.T{
					json.Wi(-stks),
					json.Wd(price),
					json.Wd(-cost),
				}
			} else {
				portfolio[nick] = []json.T{
					json.Wi(stks),
					json.Wd(price),
					json.Wd(cost),
				}
			}
		}
	}

	portfolio := map[string][]json.T{}
	for i := 0; i < investors; /*cts.go*/ i++ {
		for k, vNew := range portfolios[i] {
			if vOld, ok := portfolio[k]; ok {
				stks := vOld[0].Ri() + vNew[0].Ri()
				cost := vOld[2].Rd() + vNew[2].Rd()
				price := math.Round(cost*10000/float64(stks)) / 10000
				portfolio[k] = []json.T{
					json.Wi(stks),
					json.Wd(price),
					json.Wd(cost),
				}
				continue
			}
			portfolio[k] = vNew
		}
	}

	for k, v := range portfolio {
		entries = append(entries, json.Wa([]json.T{json.Ws(k), v[0], v[1], v[2]}))
		sum += v[2].Rd()
	}

	return
}
