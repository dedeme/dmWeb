// Copyright 04-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Accounting broker.
package brokerA

func isBigCia(nick string) bool {
	bigCias := []string{
		"AENA", "AMADEUS", "ARCELORMITTAL", "BBVA", "CAIXABANK", "CELLNEX",
		"ENDESA", "FERROVIAL", "IBERDROLA", "INDITEX", "RED ELECTRICA", "REPSOL",
		"SANTANDER", "TELEFÓNICA",
	}

	for _, n := range bigCias {
		if n == nick {
			return true
		}
	}
	return false
}

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

	big := false
	if nick != "" {
		big = isBigCia(nick)
	}

	if big {
		market = amount * 0.00003
	} else {
		if amount > 140000 {
			market = 13.4
		} else if amount > 70000 {
			market = 9.2 + amount*0.00003
		} else if amount > 35000 {
			market = 6.4 + amount*0.00007
		} else if amount > 300 {
			market = 4.65 + amount*0.00012
		} else {
			market = 1.1
		}
	}
	market += 0.11

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
