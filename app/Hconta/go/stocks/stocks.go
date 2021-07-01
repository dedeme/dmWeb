// Copyright 27-Jun-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// StocksPage javascript page
package stocks

import (
	"fmt"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/json"
)

func Process(ck string, mrq map[string]json.T) string {
	rq := cgi.RqString(mrq, "rq")
	switch rq {
	case "idata":
    lastDate := cgi.RqString(mrq, "lastDate")
		hcontaCash, hcontaSum := hcontaData()                   // hconta.go
		stocksStocks, stocksSum := stocksData()                 // stocksData
		multiMarketStocks, multiMarketCash := multiMarketData(lastDate) // multiMarket.go
		return cgi.Rp(ck, map[string]json.T{
			"hcontaCash":        json.Wd(hcontaCash),
			"hcontaSum":         json.Wd(hcontaSum),
			"stocksStocks":      json.Wa(stocksStocks),
			"stocksSum":         json.Wd(stocksSum),
			"multiMarketStocks": json.Wa(multiMarketStocks),
			"multiMarketCash":   json.Wd(multiMarketCash),
		})
	default:
		panic(fmt.Sprintf("Value of source ('%v') is not valid", rq))
	}
}