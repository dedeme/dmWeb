// Copyright 01-Jul-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Result data
package result

import (
	"github.com/dedeme/KtMMarket/cts"
	"github.com/dedeme/ktlib/js"
)

// Result data
type T struct {
	// Money
	Assets float64
	// Profits ratio
	Profits float64
	// Number of sales
	Sales float64
}

func New(assets, profits, sales float64) *T {
	return &T{assets, profits, sales}
}

func (r *T) Eval() float64 {
	assets := r.Assets
	if assets > cts.MaxAssets {
		assets = cts.MaxAssets
	}
	profits := r.Profits
	if profits > cts.MaxProfitsAvgRatio {
		profits = cts.MaxProfitsAvgRatio
	}

	return (assets*cts.AssetsRatio/cts.MaxAssets +
		(1+profits)*cts.ProfitsAvgRatio/cts.MaxProfitsAvgRatio) /
		2
}

func (r *T) Sum(r2 *T) *T {
	return New(
		r.Assets+r2.Assets,
		r.Profits+r2.Profits,
		r.Sales+r2.Sales,
	)
}

func (r *T) Div(n int) *T {
	return New(
		r.Assets/float64(n),
		r.Profits/float64(n),
		r.Sales/float64(n),
	)
}

func ToJs(r *T) string {
	return js.Wa([]string{
		js.Wd(r.Assets),
		js.Wd(r.Profits),
		js.Wd(r.Sales),
	})
}
