// Copyright 27-Jun-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// StocksPage javascript page
package stocks

import (
	"github.com/dedeme/ktlib/file"
	"github.com/dedeme/ktlib/js"
	"github.com/dedeme/ktlib/math"
)

const stocksPath = "/dm/wwwcgi/dmcgi/Stocks/data/all.tb"

// Each element in entries is: nick, stocks, price, cost
func stocksData() (entries []string, sum float64) {
	if !file.Exists(stocksPath) {
		panic(stocksPath + " not found")
	}
	yearsJs := js.Ra(file.Read(stocksPath))
	yearJs := js.Ra(yearsJs[len(yearsJs)-1])

	var portfolios []map[string][]string
	for i := 0; i < investors; /*cts.go*/ i++ {
		portfolios = append(portfolios, map[string][]string{})
	}

	for _, eJs := range js.Ra(js.Ra(yearJs[1])[1]) {
		fields := js.Ra(eJs)
		isSale := js.Rb(fields[1])
		inv := js.Ri(fields[3])
		nick := js.Rs(fields[4])
		stks := js.Ri(fields[5])
		price := js.Rd(fields[6])
		cost := price * float64(stks)

		portfolio := portfolios[inv]
		if pfe, ok := portfolio[nick]; ok {
			if isSale {
				eStocks := js.Ri(pfe[0]) - stks
				if eStocks == 0 {
					delete(portfolio, nick)
				} else {
					eCost := js.Rd(pfe[2]) - float64(stks)*js.Rd(pfe[1])
          ePrice := math.Round(eCost/float64(eStocks), 4)
					portfolio[nick] = []string{
						js.Wi(eStocks),
						js.Wd(ePrice),
						js.Wd(eCost),
					}
				}
			} else {
				eStocks := js.Ri(pfe[0]) + stks
				if eStocks == 0 {
					delete(portfolio, nick)
				} else {
					eCost := js.Rd(pfe[2]) + cost
					ePrice := math.Round(eCost/float64(eStocks), 4)
					portfolio[nick] = []string{
						js.Wi(eStocks),
						js.Wd(ePrice),
						js.Wd(eCost),
					}
				}
			}
		} else {
			if isSale {
				portfolio[nick] = []string{
					js.Wi(-stks),
					js.Wd(price),
					js.Wd(-cost),
				}
			} else {
				portfolio[nick] = []string{
					js.Wi(stks),
					js.Wd(price),
					js.Wd(cost),
				}
			}
		}
	}

	portfolio := map[string][]string{}
	for i := 0; i < investors; /*cts.go*/ i++ {
		for k, vNew := range portfolios[i] {
			if vOld, ok := portfolio[k]; ok {
				stks := js.Ri(vOld[0]) + js.Ri(vNew[0])
				cost := js.Rd(vOld[2]) + js.Rd(vNew[2])
				price := math.Round(cost/float64(stks), 4)
				portfolio[k] = []string{
					js.Wi(stks),
					js.Wd(price),
					js.Wd(cost),
				}
				continue
			}
			portfolio[k] = vNew
		}
	}

	for k, v := range portfolio {
		entries = append(entries, js.Wa([]string{js.Ws(k), v[0], v[1], v[2]}))
		sum += js.Rd(v[2])
	}

	return
}
