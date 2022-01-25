// Copyright 05-Nov-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Model broker
// It works equals to brokerA, but with a surcharge on fees.
package brokerM

import (
	"github.com/dedeme/QMarket/data/brokerA"
)

// Returns total fees of a buy or sell operation.
//    amount: Operation amount.
func Fees(amount float64) float64 {
	penalty := amount*0.0017 + 9.53
	return brokerA.Fees(amount) + penalty
}

// Returns net cost of operation (cost + fees).
//    stocks: Stocks number.
//    price  ; Stocks price.
func Buy(stocks int, price float64) float64 {
	amount := float64(stocks) * price
	tobin := amount * 0.002
	return amount + Fees(amount) + tobin
}

// Returns net incomes of operation (incomes - fees).
//    stocks: Stocks number.
//    price  ; Stocks price.
func Sell(stocks int, price float64) float64 {
	amount := float64(stocks) * price
	return amount - Fees(amount)
}
