// Copyright 27-Jun-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// StocksPage javascript page
package stocks

import (
	"fmt"
	"github.com/dedeme/ktlib/cgi"
	"github.com/dedeme/ktlib/js"
)

func Process(ck string, mrq map[string]string) string {
	rq := js.Rs(mrq["rq"]);
	switch rq {
	case "idata":
    lastDate := js.Rs(mrq["lastDate"])
		hcontaCash, hcontaSum := hcontaData()                   // hconta.go
		stocksStocks, stocksSum := stocksData()                 // stocksData
		ktMarketStocks, ktMarketCash := ktMarketData(lastDate)  // ktMarket.go
		return cgi.Rp(ck, map[string]string{
			"hcontaCash":        js.Wd(hcontaCash),
			"hcontaSum":         js.Wd(hcontaSum),
			"stocksStocks":      js.Wa(stocksStocks),
			"stocksSum":         js.Wd(stocksSum),
			"ktMarketStocks": js.Wa(ktMarketStocks),
			"ktMarketCash":   js.Wd(ktMarketCash),
		})
	default:
		panic(fmt.Sprintf("Value of source ('%v') is not valid", rq))
	}
}
