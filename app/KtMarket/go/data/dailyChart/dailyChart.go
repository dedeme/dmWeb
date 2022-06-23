// Copyright 21-Jun-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package dailyChart

import (
	"github.com/dedeme/KtMarket/cts"
	"github.com/dedeme/ktlib/arr"
	"github.com/dedeme/ktlib/js"
)

type DataT struct {
	// Investor model identifier.
	ModelId string
	// Model parameters.
	Params []float64
	// Stocks number in portfolio. 0 if the company is not en portfolio.
	Stocks int
	// Stock buy value.
	Price float64
	// Reference of buy-sell
	Ref float64
}

func NewData(
	modelId string, params []float64, stocks int, price float64, ref float64,
) *DataT {
	return &DataT{modelId, params, stocks, price, ref}
}

func dataToJs(d *DataT) string {
	return js.Wa([]string{
		js.Ws(d.ModelId),
		js.Wa(arr.Map(d.Params, js.Wd)),
		js.Wi(d.Stocks),
		js.Wd(d.Price),
		js.Wd(d.Ref),
	})
}

func dataFromJs(j string) *DataT {
	a := js.Ra(j)
	return NewData(
		js.Rs(a[0]),
		arr.Map(js.Ra(a[1]), js.Rd),
		js.Ri(a[2]),
		js.Rd(a[3]),
		js.Rd(a[4]),
	)
}

type T struct {
	// Company nick.
	Nick string
	// Previous market day close. For each investor data (d), if d.ref > close,
	// its position is to buy.
	Close float64
	// Hours of each daily quote.
	Hours []int
	// Daily quotes.
	Quotes []float64
	// Data of each investor (0...).
	InvestorsData []*DataT
}

func New(
	nk string, close float64, hours []int, quotes []float64, invsData []*DataT,
) *T {
	return &T{nk, close, hours, quotes, invsData}
}

func ToJs(d *T) string {
	return js.Wa([]string{
		js.Ws(d.Nick),
		js.Wd(d.Close),
		js.Wa(arr.Map(d.Hours, js.Wi)),
		js.Wa(arr.Map(d.Quotes, js.Wd)),
		js.Wa(arr.Map(d.InvestorsData, dataToJs)),
	})
}

func FromJs(j string) *T {
	a := js.Ra(j)
	return New(
		js.Rs(a[0]),
		js.Rd(a[1]),
		arr.Map(js.Ra(a[2]), js.Ri),
		arr.Map(js.Ra(a[3]), js.Rd),
		arr.Map(js.Ra(a[4]), dataFromJs),
	)
}

type TbT struct {
	Nicks []*T
}

func NewTb(nks []*T) *TbT {
	return &TbT{nks}
}

func Default() *TbT {
	var idata []*DataT
	for i := 0; i < cts.Investors; i++ {
		idata = append(idata, NewData("MFAKE", []float64{3.33}, 0, 0.0, 9.9))
	}
	return NewTb([]*T{
		New("FAKE", 10.0, []int{9}, []float64{11.1}, idata),
	})
}

func TbToJs(t *TbT) string {
	return js.Wa(arr.Map(t.Nicks, ToJs))
}

func TbFromJs(j string) *TbT {
	return NewTb(arr.Map(js.Ra(j), FromJs))
}
