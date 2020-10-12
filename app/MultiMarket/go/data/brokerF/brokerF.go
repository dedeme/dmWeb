// Copyright 04-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Floeas data.
package brokerF

// Returns tatal fees of a buy or sell operation.
//    amount: Operation amount.
func Fees(amount float64) float64 {
	var brk, market float64
	if amount > 25000 {
		brk = amount * 0.001
	} else {
		brk = 9.75
	}

	if amount > 140000 {
		market = 13.4
	} else if amount > 70000 {
		market = 9.2 + amount*0.00003
	} else if amount > 35000 {
		market = 6.4 + amount*0.00007
	} else {
		market = 4.65 + amount*0.00012
	}
	market += 0.11 // Execution fee

	tobin := amount * 0.002

	return brk + market + tobin
}

// Returns net cost of operation (cost + fees).
//    stocks: Stocks number.
//    price  ; Stocks price.
func Buy(stocks int, price float64) float64 {
	amount := float64(stocks) * price
	return amount + Fees(amount)
}

// Returns net incomes of operation (incomes - fees).
//    stocks: Stocks number.
//    price  ; Stocks price.
func Sell(stocks int, price float64) float64 {
	amount := float64(stocks) * price
	return amount - Fees(amount)
}
