// Copyright 08-Jun-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Accounting data.
package acc

import (
	"github.com/dedeme/KtMarket/data/broker"
	"github.com/dedeme/ktlib/arr"
	"github.com/dedeme/ktlib/js"
	"github.com/dedeme/ktlib/math"
	"github.com/dedeme/ktlib/str"
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

// Returns a fees operation or "ok=false"
func (op *OperationT) Fe() (amount float64, cause string, ok bool) {
	if op.tp == annFe {
		amount = op.f
		cause = op.s
		ok = true
	}
	return
}

// Returns a 'positive difference' operation or "ok=false"
func (op *OperationT) Pd() (amount float64, cause string, ok bool) {
	if op.tp == annPd {
		amount = op.f
		cause = op.s
		ok = true
	}
	return
}

// Returns a 'negative difference' operation or "ok=false"
func (op *OperationT) Nd() (amount float64, cause string, ok bool) {
	if op.tp == annNd {
		amount = op.f
		cause = op.s
		ok = true
	}
	return
}

func operationToJs(op *OperationT) string {
	n, s, p, ok := op.Se()
	if ok {
		return js.Wa([]string{js.Ws("se"), js.Ws(n), js.Wi(s), js.Wd(p)})
	}
	n, s, p, ok = op.Bu()
	if ok {
		return js.Wa([]string{js.Ws("bu"), js.Ws(n), js.Wi(s), js.Wd(p)})
	}
	n, s, p, ok = op.St()
	if ok {
		return js.Wa([]string{js.Ws("st"), js.Ws(n), js.Wi(s), js.Wd(p)})
	}
	a, c, ok := op.Pr()
	if ok {
		return js.Wa([]string{js.Ws("pr"), js.Wd(a), js.Ws(c)})
	}
	a, c, ok = op.Fe()
	if ok {
		return js.Wa([]string{js.Ws("fe"), js.Wd(a), js.Ws(c)})
	}
	a, c, ok = op.Pd()
	if ok {
		return js.Wa([]string{js.Ws("pd"), js.Wd(a), js.Ws(c)})
	}
	a, c, ok = op.Nd()
	if ok {
		return js.Wa([]string{js.Ws("nd"), js.Wd(a), js.Ws(c)})
	}
	a, ok = op.In()
	if ok {
		return js.Wa([]string{js.Ws("in"), js.Wd(a)})
	}
	a, ok = op.Wi()
	if ok {
		return js.Wa([]string{js.Ws("wi"), js.Wd(a)})
	}
	panic("Unknown operation")
}

func operationFromJs(j string) *OperationT {
	a := js.Ra(j)
	switch js.Rs(a[0]) {
	case "se":
		return NewSe(js.Rs(a[1]), js.Ri(a[2]), js.Rd(a[3]))
	case "bu":
		return NewBu(js.Rs(a[1]), js.Ri(a[2]), js.Rd(a[3]))
	case "st":
		return NewSt(js.Rs(a[1]), js.Ri(a[2]), js.Rd(a[3]))
	case "pr":
		return NewPr(js.Rd(a[1]), js.Rs(a[2]))
	case "fe":
		return NewFe(js.Rd(a[1]), js.Rs(a[2]))
	case "pd":
		return NewPd(js.Rd(a[1]), js.Rs(a[2]))
	case "nd":
		return NewNd(js.Rd(a[1]), js.Rs(a[2]))
	case "in":
		return NewIn(js.Rd(a[1]))
	case "wi":
		return NewWi(js.Rd(a[1]))
	}

	panic("Unkown operation '" + js.Rs(a[0]) + "'")
}

// Annotation ------------------------------------------------------------------

type AnnotationT struct {
	Id        int
	Date      string
	Operation *OperationT
}

func NewAnnotation(id int, date string, operation *OperationT) *AnnotationT {
	return &AnnotationT{id, date, operation}
}

func AnnotationToJs(a *AnnotationT) string {
	r := []string{
		js.Wi(a.Id),
		js.Ws(a.Date),
	}
	return js.Wa(append(r, js.Ra(operationToJs(a.Operation))...))
}

func AnnotationFromJs(j string) *AnnotationT {
	a := js.Ra(j)
	return &AnnotationT{
		js.Ri(a[0]),
		js.Rs(a[1]),
		operationFromJs(js.Wa(a[2:])),
	}
}

// AnnotationTbT ---------------------------------------------------------------

type AnnotationTbT struct {
	NextId      int
	Annotations []*AnnotationT
}

func NewAnnotationTb(nextId int, annotations []*AnnotationT) *AnnotationTbT {
	return &AnnotationTbT{nextId, annotations}
}

func AnnotationTbToJs(t *AnnotationTbT) string {
	return js.Wa([]string{
		js.Wi(t.NextId),
		js.Wa(arr.Map(t.Annotations, AnnotationToJs)),
	})
}

func AnnotationTbFromJs(j string) *AnnotationTbT {
	a := js.Ra(j)
	return NewAnnotationTb(
		js.Ri(a[0]),
		arr.Map(js.Ra(a[1]), AnnotationFromJs),
	)
}

// DateProf --------------------------------------------------------------------

type DateProfT struct {
	// Date of operation type 'YYYYMMDD'
	Date string
	// Functions that returns profits of operation (sell) or "ok==false" (buy)
	Profits func() (value float64, ok bool)
}

func NewSell(date string, profits float64) *DateProfT {
	return &DateProfT{date, func() (float64, bool) {
		return profits, true
	}}
}

func NewBuy(date string) *DateProfT {
	return &DateProfT{date, func() (float64, bool) {
		return 0, false
	}}
}

func DateProfToJs(dp *DateProfT) string {
	vjs := js.Wn()
	if v, ok := dp.Profits(); ok {
		vjs = js.Wd(v)
	}
	return js.Wa([]string{
		js.Ws(dp.Date),
		vjs,
	})
}

// PfEntry ---------------------------------------------------------------------

type PfEntryT struct {
	Nick   string
	Stocks int
	Price  float64 // 4 decimals
	Quote  float64 // 4 decimals
	Ref    float64 // 4 decimals
}

func NewPfEntry(
	nick string, stocks int, price float64, quote float64, ref float64,
) *PfEntryT {
	return &PfEntryT{nick, stocks, price, quote, ref}
}

func PfEntryToJs(e *PfEntryT) string {
	return js.Wa([]string{
		js.Ws(e.Nick),
		js.Wi(e.Stocks),
		js.Wd(e.Price),
		js.Wd(e.Quote),
		js.Wd(e.Ref),
	})
}

func pfAdd(pf []*PfEntryT, nick string, stocks int, price float64) []*PfEntryT {
	added := false
	r := []*PfEntryT{NewPfEntry(nick, stocks, price, -1, -1)}
	for _, e := range pf {
		if e.Nick == nick {
			r = append(r, NewPfEntry(
				nick,
				e.Stocks+stocks,
				(float64(e.Stocks)*e.Price+float64(stocks)*price)/
					float64(e.Stocks+stocks), // Not rounded to avoid errors.
				e.Quote,
				e.Ref,
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
func pfRemove(pf []*PfEntryT, nick string, stocks int) (
	r []*PfEntryT, cost float64, ok bool,
) {
	searching := true
	for _, e := range pf {
		if e.Nick == nick && searching {
			searching = false
			if e.Stocks < stocks {
				return
			}
			if e.Stocks > stocks {
				r = append(r, NewPfEntry(
					nick, e.Stocks-stocks, e.Price, e.Quote, e.Ref,
				))
			}
			cost = broker.Buy(stocks, e.Price)
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
		if en.Nick == nk {
			e = en
			ok = true
			return
		}
	}
	return
}

// Ledger ----------------------------------------------------------------------

type LedgerT struct {
	Stocks      float64 // + (activo)
	Cash        float64 // + (activo)
	Capital     float64 // - (pasivo)
	Sells       float64 // - (pasivo) sells profits
	Fees        float64 // - (pasivo)
	Profits     float64 // - (pasivo)
	Differences float64 // - (pasivo)
}

func NewLedger(
	stocks, cash, capital, sells, fees, profits, differences float64,
) *LedgerT {
	return &LedgerT{stocks, cash, capital, sells, fees, profits, differences}
}

func LedgerToJs(l *LedgerT) string {
	return js.Wa([]string{
		js.Wd(l.Stocks),
		js.Wd(l.Cash),
		js.Wd(l.Capital),
		js.Wd(l.Sells),
		js.Wd(l.Fees),
		js.Wd(l.Profits),
		js.Wd(l.Differences),
	})
}

func ledgerSell(
	l *LedgerT, stocks int, buyPrice, sellPrice float64,
) *LedgerT {
	cost := math.Round(float64(stocks)*buyPrice, 2)
	incom := math.Round(float64(stocks)*sellPrice, 2)
	fees := incom - math.Round(broker.Sell(stocks, sellPrice), 2)
	return NewLedger(
		l.Stocks-cost,
		l.Cash+incom-fees,
		l.Capital,
		l.Sells+cost-incom,
		l.Fees+fees,
		l.Profits,
		l.Differences,
	)
}

func ledgerBuy(l *LedgerT, stocks int, price float64) *LedgerT {
	if price == 0 {
		return l
	}
	cost := math.Round(float64(stocks)*price, 2)
	fees := math.Round(broker.Buy(stocks, price), 2) - cost
	return NewLedger(
		l.Stocks+cost,
		l.Cash-cost-fees,
		l.Capital,
		l.Sells,
		l.Fees+fees,
		l.Profits,
		l.Differences,
	)
}

// Returns the ledger and portfolio after settle "annotations".
//
//   annotations: Unsorted annotations.
//
// Returns:
//   ledger   : Ledger
//   portfolio: List of portfolio entries.
//   lastOps  : Map from nicks to last operation values.
//   errors   : Errors in annotations.
func Settlement(annotations []*AnnotationT) (
	ledger *LedgerT, portfolio []*PfEntryT,
	lastOps map[string]*DateProfT, serrors []string,
) {
	addErr := func(format string, values ...any) {
		serrors = append(serrors, str.Fmt(format, values...))
	}

	arr.Sort(annotations, func(ai, aj *AnnotationT) bool {
		if ai.Date < aj.Date {
			return true
		}
		if ai.Date == aj.Date {
			if ai.Operation.tp == annBu {
				return true
			}
			if aj.Operation.tp == annBu {
				return false
			}
			return ai.Id < aj.Id
		}
		return false
	})

	ledger = NewLedger(0, 0, 0, 0, 0, 0, 0)
	lastOps = map[string]*DateProfT{}

	for _, ann := range annotations {
		op := ann.Operation
		n, s, p, ok := op.Se()
		if ok {
			if s < 0 {
				addErr("%v: Selling %v (< 0) stocks", n, s)
			} else if p < 0 {
				addErr("%v: Selling price %v (< 0)", n, p)
			} else {
				pfe, ok := pfFind(portfolio, n)
				if ok {
					pf, cost, ok := pfRemove(portfolio, n, s)
					if !ok {
						addErr("%v: Selling %v stocks when in cartera there are only %v",
							n, s, pfe.Stocks,
						)
					} else {
						portfolio = pf
						ledger = ledgerSell(ledger, s, pfe.Price, p)
						lastOps[n] = NewSell(ann.Date, broker.Sell(s, p)-cost)
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
				lastOps[n] = NewBuy(ann.Date)
			}
			continue
		}

		n, s, p, ok = op.St()
		if ok {
			amount := math.Round(float64(s)*p, 2)
			ledger = NewLedger(
				ledger.Stocks+amount,
				ledger.Cash,
				ledger.Capital-amount,
				ledger.Sells,
				ledger.Fees,
				ledger.Profits,
				ledger.Differences,
			)
			portfolio = pfAdd(portfolio, n, s, p)
			continue
		}

		a, ok := op.In()
		if ok {
			ledger = NewLedger(
				ledger.Stocks,
				ledger.Cash+a,
				ledger.Capital-a,
				ledger.Sells,
				ledger.Fees,
				ledger.Profits,
				ledger.Differences,
			)
			continue
		}

		a, ok = op.Wi()
		if ok {
			ledger = NewLedger(
				ledger.Stocks,
				ledger.Cash-a,
				ledger.Capital+a,
				ledger.Sells,
				ledger.Fees,
				ledger.Profits,
				ledger.Differences,
			)
			continue
		}

		a, _, ok = op.Pr()
		if ok {
			ledger = NewLedger(
				ledger.Stocks,
				ledger.Cash+a,
				ledger.Capital,
				ledger.Sells,
				ledger.Fees,
				ledger.Profits-a,
				ledger.Differences,
			)
			continue
		}

		a, _, ok = op.Fe()
		if ok {
			ledger = NewLedger(
				ledger.Stocks,
				ledger.Cash-a,
				ledger.Capital,
				ledger.Sells,
				ledger.Fees+a,
				ledger.Profits,
				ledger.Differences,
			)
			continue
		}

		a, _, ok = op.Pd()
		if ok {
			ledger = NewLedger(
				ledger.Stocks,
				ledger.Cash+a,
				ledger.Capital,
				ledger.Sells,
				ledger.Fees,
				ledger.Profits,
				ledger.Differences-a,
			)
			continue
		}

		a, _, ok = op.Nd()
		if ok {
			ledger = NewLedger(
				ledger.Stocks,
				ledger.Cash-a,
				ledger.Capital,
				ledger.Sells,
				ledger.Fees,
				ledger.Profits,
				ledger.Differences+a,
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
	ledger, portfolio, _, serrors := Settlement(annotations)
	operations = []*OperationT{NewIn(ledger.Cash)}
	for _, e := range portfolio {
		operations = append(operations, NewSt(e.Nick, e.Stocks, e.Price))
	}
	return
}
