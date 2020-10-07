// Copyright 03-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Accounting data.
package acc

import (
	"fmt"
	"github.com/dedeme/MultiMarket/data/broker"
	"github.com/dedeme/MultiMarket/global/fn"
	"github.com/dedeme/golib/json"
	"sort"
)

// Operation -------------------------------------------------------------------

const (
	// sell
	annSe = iota
	// buy
	annBu
	// Stock
	annSt
	// Income
	annIn
	// Withdrawal
	annWi
	// Profits
	annPr
	// Fees
	annFe
	// Positive differences
	annPd
	// Negative differences
	annNd
)

type OperationT struct {
	tp int
	i  int
	f  float64
	s  string
}

// Makes a sell operation
func NewSe(nick string, stocks int, price float64) *OperationT {
	return &OperationT{annSe, stocks, price, nick}
}

// Makes a buy operation
func NewBu(nick string, stocks int, price float64) *OperationT {
	return &OperationT{annBu, stocks, price, nick}
}

// Makes a stock operation
func NewSt(nick string, stocks int, price float64) *OperationT {
	return &OperationT{annSt, stocks, price, nick}
}

// Makes an income operation
func NewIn(amount float64) *OperationT {
	return &OperationT{annIn, 0, amount, ""}
}

// Makes a withdrawal operation
func NewWi(amount float64) *OperationT {
	return &OperationT{annWi, 0, amount, ""}
}

// Makes a profits operation
func NewPr(amount float64, cause string) *OperationT {
	return &OperationT{annPr, 0, amount, cause}
}

// Makes a fees operation
func NewFe(amount float64, cause string) *OperationT {
	return &OperationT{annFe, 0, amount, cause}
}

// Makes a 'positive differences' operation
func NewPd(amount float64, cause string) *OperationT {
	return &OperationT{annPd, 0, amount, cause}
}

// Makes a 'negative differences' operation
func NewNd(amount float64, cause string) *OperationT {
	return &OperationT{annNd, 0, amount, cause}
}

// Returns a sell operation or "ok=false"
func (op *OperationT) Se() (nick string, stocks int, price float64, ok bool) {
	if op.tp == annSe {
		nick = op.s
		stocks = op.i
		price = op.f
		ok = true
	}
	return
}

// Returns a buy operation or "ok=false"
func (op *OperationT) Bu() (nick string, stocks int, price float64, ok bool) {
	if op.tp == annBu {
		nick = op.s
		stocks = op.i
		price = op.f
		ok = true
	}
	return
}

// Returns a stock operation or "ok=false"
func (op *OperationT) St() (nick string, stocks int, price float64, ok bool) {
	if op.tp == annSt {
		nick = op.s
		stocks = op.i
		price = op.f
		ok = true
	}
	return
}

// Returns an income operation or "ok=false"
func (op *OperationT) In() (amount float64, ok bool) {
	if op.tp == annIn {
		amount = op.f
		ok = true
	}
	return
}

// Returns a withdrawal operation or "ok=false"
func (op *OperationT) Wi() (amount float64, ok bool) {
	if op.tp == annWi {
		amount = op.f
		ok = true
	}
	return
}

// Returns a profits operation or "ok=false"
func (op *OperationT) Pr() (amount float64, cause string, ok bool) {
	if op.tp == annPr {
		amount = op.f
		cause = op.s
		ok = true
	}
	return
}

// Returns a Fees operation or "ok=false"
func (op *OperationT) Fe() (amount float64, cause string, ok bool) {
	if op.tp == annFe {
		amount = op.f
		cause = op.s
		ok = true
	}
	return
}

// Returns a profits operation or "ok=false"
func (op *OperationT) Pd() (amount float64, cause string, ok bool) {
	if op.tp == annPd {
		amount = op.f
		cause = op.s
		ok = true
	}
	return
}

// Returns a profits operation or "ok=false"
func (op *OperationT) Nd() (amount float64, cause string, ok bool) {
	if op.tp == annNd {
		amount = op.f
		cause = op.s
		ok = true
	}
	return
}

func (op *OperationT) toJs() json.T {
	n, s, p, ok := op.Se()
	if ok {
		return json.Wa([]json.T{json.Ws("se"), json.Ws(n), json.Wi(s), json.Wd(p)})
	}
	n, s, p, ok = op.Bu()
	if ok {
		return json.Wa([]json.T{json.Ws("bu"), json.Ws(n), json.Wi(s), json.Wd(p)})
	}
	n, s, p, ok = op.St()
	if ok {
		return json.Wa([]json.T{json.Ws("st"), json.Ws(n), json.Wi(s), json.Wd(p)})
	}
	a, c, ok := op.Pr()
	if ok {
		return json.Wa([]json.T{json.Ws("pr"), json.Wd(a), json.Ws(c)})
	}
	a, c, ok = op.Fe()
	if ok {
		return json.Wa([]json.T{json.Ws("fe"), json.Wd(a), json.Ws(c)})
	}
	a, c, ok = op.Pd()
	if ok {
		return json.Wa([]json.T{json.Ws("pd"), json.Wd(a), json.Ws(c)})
	}
	a, c, ok = op.Nd()
	if ok {
		return json.Wa([]json.T{json.Ws("nd"), json.Wd(a), json.Ws(c)})
	}
	a, ok = op.In()
	if ok {
		return json.Wa([]json.T{json.Ws("in"), json.Wd(a)})
	}
	a, ok = op.Wi()
	if ok {
		return json.Wa([]json.T{json.Ws("wi"), json.Wd(a)})
	}
	panic("Unknown operation")
}

func operationFromJs(js json.T) *OperationT {
	a := js.Ra()
	switch a[0].Rs() {
	case "se":
		return NewSe(a[1].Rs(), a[2].Ri(), a[3].Rd())
	case "bu":
		return NewBu(a[1].Rs(), a[2].Ri(), a[3].Rd())
	case "st":
		return NewSt(a[1].Rs(), a[2].Ri(), a[3].Rd())
	case "pr":
		return NewPr(a[1].Rd(), a[2].Rs())
	case "fe":
		return NewFe(a[1].Rd(), a[2].Rs())
	case "pd":
		return NewPd(a[1].Rd(), a[2].Rs())
	case "nd":
		return NewNd(a[1].Rd(), a[2].Rs())
	case "in":
		return NewIn(a[1].Rd())
	case "wi":
		return NewWi(a[1].Rd())
	}

	panic("Unkown operation '" + a[0].Rs() + "'")
}

// Annotation ------------------------------------------------------------------

type AnnotationT struct {
	id        int
	date      string
	operation *OperationT
}

func NewAnnotation(id int, date string, operation *OperationT) *AnnotationT {
	return &AnnotationT{id, date, operation}
}

func (a *AnnotationT) Id() int {
	return a.id
}

func (a *AnnotationT) Date() string {
	return a.date
}

func (a *AnnotationT) Operation() *OperationT {
	return a.operation
}

func (a *AnnotationT) ToJs() json.T {
	r := []json.T{
		json.Wi(a.id),
		json.Ws(a.date),
	}
	return json.Wa(append(r, a.operation.toJs().Ra()...))
}

func AnnotationFromJs(js json.T) *AnnotationT {
	a := js.Ra()
	return &AnnotationT{
		a[0].Ri(),
		a[1].Rs(),
		operationFromJs(json.Wa(a[2:])),
	}
}

// PfEntry ---------------------------------------------------------------------

type PfEntryT struct {
	nick   string
	stocks int
	price  float64 // 4 decimals
	quote  float64 // 4 decimals
	ref    float64 // 4 decimals
}

func NewPfEntry(
	nick string, stocks int, price float64, quote float64, ref float64,
) *PfEntryT {
	return &PfEntryT{nick, stocks, price, quote, ref}
}

func (e *PfEntryT) Nick() string {
	return e.nick
}

func (e *PfEntryT) Stocks() int {
	return e.stocks
}

func (e *PfEntryT) Price() float64 {
	return e.price
}

func (e *PfEntryT) Quote() float64 {
	return e.quote
}

func (e *PfEntryT) Ref() float64 {
	return e.ref
}

func (e *PfEntryT) ToJs() json.T {
	return json.Wa([]json.T{
		json.Ws(e.nick),
		json.Wi(e.stocks),
		json.Wd(e.price),
		json.Wd(e.quote),
		json.Wd(e.ref),
	})
}

func pfAdd(pf []*PfEntryT, nick string, stocks int, price float64) []*PfEntryT {
	added := false
	r := []*PfEntryT{NewPfEntry(nick, stocks, price, -1, -1)}
	for _, e := range pf {
		if e.nick == nick {
			r = append(r, NewPfEntry(
				nick,
				e.stocks+stocks,
				(float64(e.stocks)*e.price+float64(stocks)*price)/
					float64(e.stocks+stocks), // Not rounded to avoid errors.
				e.quote,
				e.ref,
			))
			added = true
			continue
		}
		r = append(r, e)
	}
	if added {
		return r[1:]
	}
	return r
}

// stocks must be less or equals to pf.stocks. Otherwise 'ok == false'.
func pfRemove(pf []*PfEntryT, nick string, stocks int) (r []*PfEntryT, ok bool) {
	for _, e := range pf {
		if e.nick == nick {
			if e.stocks < stocks {
				return
			}
			if e.stocks > stocks {
				r = append(r, NewPfEntry(
					nick, e.stocks-stocks, e.price, e.quote, e.ref,
				))
			}
			continue
		}
		r = append(r, e)
	}
	ok = true
	return
}

// Finds the entry of nick 'nk' or returns 'ok=false'
func pfFind(pf []*PfEntryT, nk string) (e *PfEntryT, ok bool) {
	for _, en := range pf {
		if en.nick == nk {
			e = en
			ok = true
			return
		}
	}
	return
}

// Ledger ----------------------------------------------------------------------

type LedgerT struct {
	stocks      float64 // + (activo)
	cash        float64 // + (activo)
	capital     float64 // - (pasivo)
	sells       float64 // - (pasivo) sells profits
	fees        float64 // - (pasivo)
	profits     float64 // - (pasivo)
	differences float64 // - (pasivo)
}

func NewLedger(
	stocks, cash, capital, sells, fees, profits, differences float64,
) *LedgerT {
	return &LedgerT{stocks, cash, capital, sells, fees, profits, differences}
}

func (l *LedgerT) Stocks() float64 {
	return l.stocks
}

func (l *LedgerT) Cash() float64 {
	return l.cash
}

func (l *LedgerT) Capital() float64 {
	return l.capital
}

func (l *LedgerT) Sells() float64 {
	return l.sells
}

func (l *LedgerT) Fees() float64 {
	return l.fees
}

func (l *LedgerT) Profits() float64 {
	return l.profits
}

func (l *LedgerT) Differences() float64 {
	return l.differences
}

func (l *LedgerT) ToJs() json.T {
	return json.Wa([]json.T{
		json.Wd(l.stocks),
		json.Wd(l.cash),
		json.Wd(l.capital),
		json.Wd(l.sells),
		json.Wd(l.fees),
		json.Wd(l.profits),
		json.Wd(l.differences),
	})
}

func ledgerSell(l *LedgerT, stocks int, buyPrice, sellPrice float64) *LedgerT {
	cost := fn.Fix(float64(stocks)*buyPrice, 2)
	incom := fn.Fix(float64(stocks)*sellPrice, 2)
	fees := fn.Fix(broker.Fees(incom), 2)
	return NewLedger(
		l.stocks-cost,
		l.cash+incom-fees,
		l.capital,
		l.sells+cost-incom,
		l.fees+fees,
		l.profits,
		l.differences,
	)
}

func ledgerBuy(l *LedgerT, stocks int, price float64) *LedgerT {
	if price == 0 {
		return l
	}
	cost := fn.Fix(float64(stocks)*price, 2)
	fees := fn.Fix(broker.Fees(cost), 2)
	return NewLedger(
		l.stocks+cost,
		l.cash-cost-fees,
		l.capital,
		l.sells,
		l.fees+fees,
		l.profits,
		l.differences,
	)
}

// Returns the ledger and portfolio after settle "annotations".
//
//   annotations: Unsorted annotations.
//
// Returns:
//   ledger   : Ledger
//   portfolio: List of portfolio entries.
//   errors   : Errors in annotations.
func Settlement(annotations []*AnnotationT) (
	ledger *LedgerT, portfolio []*PfEntryT, serrors []string,
) {
	addErr := func(format string, values ...interface{}) {
		serrors = append(serrors, fmt.Sprintf(format, values))
	}

	sort.Slice(annotations, func(i, j int) bool {
		ai := annotations[i]
		aj := annotations[j]
		if ai.date < aj.date {
			return true
		}
		if ai.date == aj.date {
			if ai.operation.tp == annBu {
				return true
			}
			if aj.operation.tp == annBu {
				return false
			}
			return ai.id < aj.id
		}
		return false
	})

	ledger = NewLedger(0, 0, 0, 0, 0, 0, 0)

	for _, ann := range annotations {
		op := ann.operation
		n, s, p, ok := op.Se()
		if ok {
			if s < 0 {
				addErr("%v: Selling %v (< 0) stocks", n, s)
			} else if p < 0 {
				addErr("%v: Selling price %v (< 0)", n, p)
			} else {
				pfe, ok := pfFind(portfolio, n)
				if ok {
					pf, ok := pfRemove(portfolio, n, s)
					if !ok {
						addErr("%v: Selling %v stocks when in cartera there are only %v",
							n, s, pfe.stocks,
						)
					} else {
						portfolio = pf
						ledger = ledgerSell(ledger, s, pfe.price, p)
					}
				} else {
					addErr("%v: Nick is missing in portfolio", n)
				}
			}
			continue
		}

		n, s, p, ok = op.Bu()
		if ok {
			if s <= 0 {
				addErr("%v: Buing %s (<= 0) stocks", n, s)
			} else if p < 0 {
				addErr("%v: Buing price %v (< 0)", n, p)
			} else {
				portfolio = pfAdd(portfolio, n, s, p)
				ledger = ledgerBuy(ledger, s, p)
			}
			continue
		}

		n, s, p, ok = op.St()
		if ok {
			amount := fn.Fix(float64(s)*p, 2)
			ledger = NewLedger(
				ledger.stocks+amount,
				ledger.cash,
				ledger.capital-amount,
				ledger.sells,
				ledger.fees,
				ledger.profits,
				ledger.differences,
			)
			portfolio = pfAdd(portfolio, n, s, p)
			continue
		}

		a, ok := op.In()
		if ok {
			ledger = NewLedger(
				ledger.stocks,
				ledger.cash+a,
				ledger.capital-a,
				ledger.sells,
				ledger.fees,
				ledger.profits,
				ledger.differences,
			)
			continue
		}

		a, ok = op.Wi()
		if ok {
			ledger = NewLedger(
				ledger.stocks,
				ledger.cash-a,
				ledger.capital+a,
				ledger.sells,
				ledger.fees,
				ledger.profits,
				ledger.differences,
			)
			continue
		}

		a, _, ok = op.Pr()
		if ok {
			ledger = NewLedger(
				ledger.stocks,
				ledger.cash+a,
				ledger.capital,
				ledger.sells,
				ledger.fees,
				ledger.profits-a,
				ledger.differences,
			)
			continue
		}

		a, _, ok = op.Fe()
		if ok {
			ledger = NewLedger(
				ledger.stocks,
				ledger.cash-a,
				ledger.capital,
				ledger.sells,
				ledger.fees+a,
				ledger.profits,
				ledger.differences,
			)
			continue
		}

		a, _, ok = op.Pd()
		if ok {
			ledger = NewLedger(
				ledger.stocks,
				ledger.cash+a,
				ledger.capital,
				ledger.sells,
				ledger.fees,
				ledger.profits,
				ledger.differences-a,
			)
			continue
		}

		a, _, ok = op.Nd()
		if ok {
			ledger = NewLedger(
				ledger.stocks,
				ledger.cash-a,
				ledger.capital,
				ledger.sells,
				ledger.fees,
				ledger.profits,
				ledger.differences+a,
			)
			continue
		}

		addErr("Unknown annotation '%v'", ann)
	}

	return
}

// Returns operations to open a new year.
//    annotations: Unsorted annotations.
// Returns:
//    operations: List of operations.
//    errors    : Errors in annotations.
func Regularize(
	annotations []*AnnotationT,
) (operations []*OperationT, serrors []string) {
	ledger, portfolio, serrors := Settlement(annotations)
	operations = []*OperationT{NewIn(ledger.cash)}
	for _, e := range portfolio {
		operations = append(operations, NewSt(e.nick, e.stocks, e.price))
	}
	return
}
