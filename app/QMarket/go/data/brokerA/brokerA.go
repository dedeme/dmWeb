// Copyright 05-Nov-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Accounting broker.
package brokerA

// Returns total fees of a buy or sell operation.
//    nick  : Company nick. If nick is blank, it will be processed as a not
//            big company.
//    amount: Operation amount.
func Fees(nick string, amount float64) float64 {
	var brk, market float64
	if amount > 50000 {
		brk = amount * 0.001
	} else {
		brk = 9.75
	}

	market = amount * 0.00003
	market += 0.11 // Execution fee

	return brk + market
}

// Returns net cost of operation (cost + fees).
//    nick  : Company nick.
//    stocks: Stocks number.
//    price  ; Stocks price.
func Buy(nick string, stocks int, price float64) float64 {
	amount := float64(stocks) * price
	tobin := amount * 0.002

	return amount + Fees(nick, amount) + tobin
}

// Returns net incomes of operation (incomes - fees).
//    nick  : Company nick.
//    stocks: Stocks number.
//    price : Stocks price.
func Sell(nick string, stocks int, price float64) float64 {
	amount := float64(stocks) * price
	return amount - Fees(nick, amount)
}
