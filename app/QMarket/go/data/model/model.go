// Copyright 17-Oct-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Bet system
package model

import (
	"github.com/dedeme/QMarket/data/cts"
	"github.com/dedeme/QMarket/data/model/eval"
	"github.com/dedeme/QMarket/data/model/heval"
	"github.com/dedeme/QMarket/data/qtable"
	"github.com/dedeme/golib/date"
	"github.com/dedeme/golib/json"
	"sort"
)

type T struct {
	qlevel      int
	id          int
	param       float64
	evaluation  *eval.T
	hevaluation *heval.T
}

// Creates a new model with 'evaluation' and 'hevaluation' set to 'nil'
func New(qlevel int, id int) *T {
	return &T{qlevel, id, float64(id) / cts.RangesToParam, nil, nil}
}

// Returns the quantum level of 'm'.
func (m *T) Qlevel() int {
	return m.qlevel
}

// Returns the identifier of 'm'
func (m *T) Id() int {
	return m.id
}

// Returns the parameter of 'm'.
func (m *T) Param() float64 {
	return m.param
}

// Returns the evaluation of 'm' or 'ok=false' if 'm' was not evaluated.
func (m *T) Evaluation() (ok bool, ev *eval.T) {
	if m.evaluation != nil {
		ok = true
		ev = m.evaluation
	}
	return
}

// Returns the historic evaluation of 'm' or 'ok=false' if 'm' was not
// evaluated.
func (m *T) Hevaluation() (ok bool, hev *heval.T) {
	if m.hevaluation != nil {
		ok = true
		hev = m.hevaluation
	}
	return
}

// Evaluate 'm' appending a new evalutation to the historic.
//
// Values of 'm.evaluation' and 'm.hevaluation' are recalculated. The last one
// by adding new values from 'm.evaluation'.
//    days    : Weight of historic evaluation.
//    oldSales: Old number of sales.
//    oldValue: Old evaluation value.
//    opens   : Opens table.
//    closes  : Closes table.
func (m *T) AppendEvaluation(
	days int, oldSales, oldValue float64, opens, closes *qtable.T,
) {
	buys, sales, assets := m.Assets(opens, closes)
	avg := m.ProfitsAvg(opens, closes)
	m.evaluation = eval.New(buys, sales, assets, avg)
	m.hevaluation = heval.New(
		(oldSales*float64(days)+float64(sales))/float64(days+1),
		(oldValue*float64(days)+m.evaluation.Value())/float64(days+1),
	)
}

// Modify the last evaluation of 'm' with a new evaluation.
//
// Values of 'm.evaluation' and 'm.hevaluation' are recalculated. The last one
// by removing old 'm.evaluation' values and adding its new values.
//    days  : Weight current historic evaluation.
//    result: Results of last historic evaluation.
//    opens : Opens table.
//    closes: Closes table.
func (m *T) ModifyEvaluation(
	days int, result *RsT, opens, closes *qtable.T,
) {
	oldSales :=
		(result.historicSales*float64(days) - float64(result.lastSales)) /
			float64(days-1)
	oldValue :=
		(result.historicValue*float64(days) - result.lastValue) / float64(days-1)

	m.AppendEvaluation(days-1, oldSales, oldValue, opens, closes)
}

// Evaluate 'm' when there is not any previous historic evaluation.
//
// Values of 'm.evaluation' are recalculated, and values of 'm.hevaluation' are
// copied from 'm.evalation'.
//    opens : Opens table.
//    closes: Closes table.
func (m *T) FirstEvaluation(opens, closes *qtable.T) {
	buys, sales, assets := m.Assets(opens, closes)
	avg := m.ProfitsAvg(opens, closes)
	m.evaluation = eval.New(buys, sales, assets, avg)
	m.hevaluation = heval.New(float64(sales), m.evaluation.Value())
}

// Sets evaluations of 'm'.
//
// Values of 'm.evaluation' are recalculated, but values of 'm.hevaluation' are
// copied from 'result'.
//    result: Results of last historic evaluation.
//    opens : Opens table.
//    closes: Closes table.
func (m *T) SetEvaluation(result *RsT, opens, closes *qtable.T) {
	buys, sales, assets := m.Assets(opens, closes)
	avg := m.ProfitsAvg(opens, closes)
	m.evaluation = eval.New(buys, sales, assets, avg)
	m.hevaluation = heval.New(result.historicSales, result.historicValue)
}

func (m *T) ToJs() json.T {
	if m.evaluation == nil {
		panic("m.evaluation is nil")
	}
	if m.hevaluation == nil {
		panic("m.hevaluation is nil")
	}
	return json.Wa([]json.T{
		json.Wi(m.qlevel),
		json.Wi(m.id),
		m.evaluation.ToJs(),
		m.hevaluation.ToJs(),
	})
}

func FromJs(js json.T) *T {
	a := js.Ra()
	id := a[1].Ri()
	return &T{
		a[0].Ri(),
		id,
		float64(id) / cts.RangesToParam,
		eval.FromJs(a[2]),
		heval.FromJs(a[3]),
	}
}

type modelOrder []*T

func (a modelOrder) Len() int { return len(a) }

func (a modelOrder) Swap(i, j int) { a[i], a[j] = a[j], a[i] }

func (a modelOrder) Less(i, j int) bool {
	return a[i].hevaluation.Value() > a[j].hevaluation.Value()
}

// Sorts 'models' by its 'hevaluation.Value()' from more to less value.
func Sort(models []*T) {
	sort.Sort(modelOrder(models))
}

// RsT -------------------------------------------------------------------------

type RsT struct {
	lastSales     int
	lastValue     float64
	historicSales float64
	historicValue float64
}

func NewRs(
	lastSales int, lastValue float64,
	historicSales float64, historicValue float64,
) *RsT {
	return &RsT{lastSales, lastValue, historicSales, historicValue}
}

func (rs *RsT) LastSales() int {
	return rs.lastSales
}

func (rs *RsT) LastValue() float64 {
	return rs.lastValue
}

func (rs *RsT) HistoricSales() float64 {
	return rs.historicSales
}

func (rs *RsT) HistoricValue() float64 {
	return rs.historicValue
}

func (rs *RsT) ToJs() json.T {
	return json.Wa([]json.T{
		json.Wi(rs.lastSales),
		json.Wi(int(rs.lastValue * 1000000)),
		json.Wi(int(rs.historicSales * 1000)),
		json.Wi(int(rs.historicValue * 1000000)),
	})
}

func RsFromJs(js json.T) *RsT {
	a := js.Ra()
	return &RsT{
		a[0].Ri(),
		float64(a[1].Ri()) / 1000000,
		float64(a[2].Ri()) / 1000,
		float64(a[3].Ri()) / 1000000,
	}
}

// TableT ----------------------------------------------------------------------

type TableT struct {
	date    string
	days    int
	results map[int][]*RsT
}

func NewTable(date string, days int, results map[int][]*RsT) *TableT {
	return &TableT{date, days, results}
}

func NewEmptyTable() *TableT {
	return &TableT{date.Now().String(), 0, map[int][]*RsT{}}
}

// Returns the date of calculus.
func (t *TableT) Date() string {
	return t.date
}

// Returns the days for historic average
func (t *TableT) Days() int {
	return t.days
}

// Returns results of each parameter.
//
// For each parameter there are cts.Qlevel results (*RsT).
func (t *TableT) Results() map[int][]*RsT {
	return t.results
}

func (t *TableT) ToJs() json.T {
	var mrss []json.T
	for k, rss := range t.results {
		rowJs := []json.T{json.Wi(k)}
		for _, rs := range rss {
			rowJs = append(rowJs, rs.ToJs())
		}
		mrss = append(mrss, json.Wa(rowJs))
	}
	return json.Wa([]json.T{
		json.Ws(t.date),
		json.Wi(t.days),
		json.Wa(mrss),
	})
}

func TableFromJs(js json.T) *TableT {
	a := js.Ra()
	mrss := map[int][]*RsT{}
	for _, rowJs := range a[2].Ra() {
		fieldsJs := rowJs.Ra()
		k := fieldsJs[0].Ri()
		var v []*RsT
		for i := 1; i < len(fieldsJs); i++ {
			v = append(v, RsFromJs(fieldsJs[i]))
		}
		mrss[k] = v
	}
	return &TableT{
		a[0].Rs(),
		a[1].Ri(),
		mrss,
	}
}
