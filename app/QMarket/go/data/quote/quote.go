// Copyright 19-Oct-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Quote data.
package quote

import (
	"errors"
	"fmt"
	"github.com/dedeme/QMarket/data/cts"
	"github.com/dedeme/golib/json"
	"sort"
	"strconv"
	"strings"
	"time"
)

// T ---------------------------------------------------------------------------

// Quote type
type T struct {
	date  string
	open  float64
	close float64
	max   float64
	min   float64
	vol   int
	err   bool
}

// Create a new quote.
//    date : Date of quote in format "YYYYMMDD".
//    open : Open value.
//    close: Close value.
//    max  : Maximum value.
//    min  : Minimum value.
//    vol  : Valume value.
//    err  : 'true' if quote was modified manually.
func New(date string, open, close, max, min float64, vol int, err bool) *T {
	return &T{date, open, close, max, min, vol, err}
}

func (q *T) Date() string {
	return q.date
}

func (q *T) Open() float64 {
	return q.open
}

func (q *T) Close() float64 {
	return q.close
}

func (q *T) Max() float64 {
	return q.max
}

func (q *T) Min() float64 {
	return q.min
}

func (q *T) Vol() int {
	return q.vol
}

func (q *T) Err() bool {
	return q.err
}

func (q *T) ToJs() json.T {
	return json.Wa([]json.T{
		json.Ws(q.date),
		json.Wd(q.open),
		json.Wd(q.close),
		json.Wd(q.max),
		json.Wd(q.min),
		json.Wi(q.vol),
		json.Wb(q.err),
	})
}

func FromJs(js json.T) *T {
	a := js.Ra()
	return &T{
		a[0].Rs(),
		a[1].Rd(),
		a[2].Rd(),
		a[3].Rd(),
		a[4].Rd(),
		a[5].Ri(),
		a[6].Rb(),
	}
}

// Returns a string representation of "q".
//    q: Quote.
func (q *T) String() string {
	err := "false"
	if q.err {
		err = "true"
	}
	return q.date + ":" +
		fmt.Sprintf("%.4f", q.open) + ":" +
		fmt.Sprintf("%.4f", q.close) + ":" +
		fmt.Sprintf("%.4f", q.max) + ":" +
		fmt.Sprintf("%.4f", q.min) + ":" +
		fmt.Sprintf("%d", q.vol) + ":" +
		err
}

// Returns a Quote from its string representation. If the conversion fails
// returns 'ok=false'
//    s: Quote representation.
func FromString(s string) (q *T, ok bool) {
	a := strings.Split(s, ":")
	if len(a) != 7 {
		return
	}
	for i := range a {
		a[i] = strings.TrimSpace(a[i])
	}

	_, err := time.Parse("20060102", a[0])
	if err != nil {
		return
	}
	open, oerr := strconv.ParseFloat(a[1], 64)
	close, cerr := strconv.ParseFloat(a[2], 64)
	max, xerr := strconv.ParseFloat(a[3], 64)
	min, nerr := strconv.ParseFloat(a[4], 64)
	vol, verr := strconv.Atoi(a[5])
	if oerr != nil || cerr != nil || xerr != nil || nerr != nil || verr != nil {
		return
	}
	e := false
	if a[6] == "true" {
		e = true
	} else if a[6] != "false" {
		return
	}

	q = &T{a[0], open, close, max, min, vol, e}
	ok = true
	return
}

// Returns quotes from ones in text format. Empty lines are skipped.
//    qsTx: Quotes in string format.
func TxToQs(qsTx string) (qs []*T, err error) {
	for _, s := range strings.Split(qsTx, "\n") {
		if s == "" {
			continue
		}
		q, ok := FromString(s)
		if !ok {
			err = errors.New("Wrong quote: '" + s + "'")
			return
		}
		qs = append(qs, q)
	}
	if len(qs) != cts.HistoricQuotes {
		err = errors.New(fmt.Sprintf(
			"Wrong quotes number (%v != %v).", len(qs), cts.HistoricQuotes,
		))
	}
	return
}

// Returns the number of manual corrections in qs.
//    qs: Quotes
func Manuals(qs []*T) (n int) {
	for _, e := range qs {
		if e.err {
			n++
		}
	}
	return
}

// Checks maximum and minimum and returns a new quote corrected, if it is
// necessary.
//
// If q.err = true, quote will not be corrected.
//
// If quote was corrected, its 'err' field is set to 'true'.
//
//    q: Quote to correct.
//    ----
//    Returns:
//      newQ : Corrected quote.
//      errs : List of corrections (e.g. Close > Max).
func Correct1(q *T) (newQ *T, errs []error) {
	newQ = q
	if q.err {
		return
	}
	date := q.date
	open := q.open
	close := q.close
	max := q.max
	min := q.min
	vol := q.vol

	if open > max {
		max = open
		errs = append(errs, errors.New("Open > Max"))
	}
	if close > max {
		max = close
		errs = append(errs, errors.New("Close > Max"))
	}
	if open < min {
		min = open
		errs = append(errs, errors.New("Open < Min"))
	}
	if close < min {
		min = close
		errs = append(errs, errors.New("Close < Min"))
	}

	if len(errs) > 0 {
		newQ = &T{date, open, close, max, min, vol, true}
	}
	return
}

// Checks maximum and minimum and returns a new quote corrected, if it is
// necessary.
//
// If q.err = true, quote will not be corrected.
//
// If quote was corrected, its 'err' field is set to 'true'.
//
//    last    : Quote to correct.
//    previous: Quote previous to "last"
//    ----
//    Returns:
//      newQ : Corrected quote.
//      errs : List of corrections (e.g. Close > Max).
func Correct2(last, previous *T) (newQ *T, errs []error) {
	newQ = last
	if last.err {
		return
	}
	date := last.date
	open := last.open
	close := last.close
	max := last.max
	min := last.min
	vol := last.vol

	open0 := previous.open
	close0 := previous.close
	max0 := previous.max
	min0 := previous.min

	if max0 < min0 || open0 < 0 {
		newQ, errs = Correct1(last)
		return
	}

	if open > max {
		if open == open0 && max != max0 {
			open = max
		} else {
			max = open
		}
		errs = append(errs, errors.New("Open > Max"))
	}
	if close > max {
		if close == close0 && max != max0 {
			close = max
		} else {
			max = close
		}
		errs = append(errs, errors.New("Close > Max"))
	}
	if open < min {
		if open == open0 && min != min0 {
			open = min
		} else {
			min = open
		}
		errs = append(errs, errors.New("Open < Min"))
	}
	if close < min {
		if close == close0 && min != min0 {
			close = min
		} else {
			min = close
		}
		errs = append(errs, errors.New("Close < Min"))
	}

	if len(errs) > 0 {
		newQ = &T{date, open, close, max, min, vol, true}
	}
	return
}

// Checks increment (+-20%) and returns a new quote corrected, if it is
// necessary.
//
// If q.err = true, quote will not be corrected.
//
// If quote was corrected, its 'err' field is set to 'true'.
//
//    last    : Quote to correct.
//    previous: Quote previous to "last"
//    ----
//    Returns:
//      newQ : Corrected quote (only 'Err' field).
//      errs : List of corrections (e.g. Close +20%).
func Correct3(last, previous *T) (newQ *T, errs []error) {
	newQ = last
	if last.err {
		return
	}
	date := last.date
	open := last.open
	close := last.close
	max := last.max
	min := last.min
	vol := last.vol

	open0 := previous.open
	close0 := previous.close
	max0 := previous.max
	min0 := previous.min

	if open0 < 0 {
		return
	}

	if open > open0*1.2 {
		errs = append(errs, errors.New("Open +20%"))
	}
	if close > close0*1.2 {
		errs = append(errs, errors.New("Close +20%"))
	}
	if max > max0*1.2 {
		errs = append(errs, errors.New("Max +20%"))
	}
	if min > min0*1.2 {
		errs = append(errs, errors.New("Min +20%"))
	}

	if open < open0*0.8 {
		errs = append(errs, errors.New("Open -20%"))
	}
	if close < close0*0.8 {
		errs = append(errs, errors.New("Close -20%"))
	}
	if max < max0*0.8 {
		errs = append(errs, errors.New("Max -20%"))
	}
	if min < min0*0.8 {
		errs = append(errs, errors.New("Min -20%"))
	}

	if len(errs) > 0 {
		newQ = &T{date, open, close, max, min, vol, true}
	}
	return
}

// Corrects "qs" and returns a list with corrected quotes.
//    qs: Historic quotes.
//    ----
//    Returns:
//      newQs  : Quotes corrected, if necessary.
//      errs: A list with errors coming from correct1, correct2 and
//            correct3. If there was no error, it is an empty list.
func Correct(qs []*T) (newQs []*T, errs []error) {
	mkErrs := func(q *T, es []error) []error {
		var r []error
		for _, e := range es {
			r = append(r, errors.New(q.date+": "+e.Error()))
		}
		return r
	}

	if len(qs) == 0 {
		return
	}

	q2 := qs[0]
	for i := 1; i < len(qs); i++ {
		q1 := q2
		q2 = qs[i]
		q, e := Correct2(q1, q2)
		errs = append(errs, mkErrs(q, e)...)
		q, e = Correct3(q, q2)
		errs = append(errs, mkErrs(q, e)...)

		newQs = append(newQs, q)
	}
	q, e := Correct1(q2)
	errs = append(errs, mkErrs(q, e)...)
	q, e = Correct3(q, q2)
	errs = append(errs, mkErrs(q, e)...)

	newQs = append(newQs, q)

	return
}

// Corrects "qs" and returns a list with corrected quotes.
//    mqs: Historic quotes of company model
//    qs : Historic quotes of company.
//    ----
//    Returns:
//      newQs  : Quotes corrected, if necessary.
//      errs: Errors by extra o missing quotes. If there was no errors,
//               it is an empty list.
func CorrecDates(mqs, qs []*T) (newQs []*T, errs []error) {
	mi := 0
	lmqs := len(mqs)
	i := 0
	lqs := len(qs)
	for {
		if mi < lmqs {
			mq := mqs[mi]
			if i < lqs {
				q := qs[i]
				if mq.date > q.date {
					errs = append(errs, errors.New(mq.date+": "+"Missing quote"))
					newQs = append(newQs, &T{mq.date, -1, -1, -1, -1, -1, true})
					mi++
					continue
				}
				if mq.date < q.date {
					errs = append(errs, errors.New(q.date+": "+"Extra quote"))
					i++
					continue
				}
				newQs = append(newQs, q)
				i++
				mi++
				continue
			}
			errs = append(errs, errors.New(mq.date+": "+"Missing quote"))
			newQs = append(newQs, &T{mq.date, -1, -1, -1, -1, -1, true})
			mi++
			continue
		}
		if i < lqs {
			q := qs[i]
			errs = append(errs, errors.New(q.date+": "+"Extra quote"))
			i++
			continue
		}
		break
	}
	return
}

// Merges new quotes with others already existent.
//    model: Model quotes. When quotes are of company model, it is an
//           empty array.
//    newQs: Last quotes read from the Internet. It can not be empty.
//    oldQs: Existent quotes in file system. It can not be empty.
//    ----
//    Returns
//      qs  : Array made with the following process:
//            1. Every quote on top with 'open = -1' is removed from "oldQs"
//               in the dates range of "newQs"
//            2. If there are new and old quotes for the same date, that of
//               'old' is selected.
//            3. The return array is corrected in the range of 'new' dates
//               and added or removed quotes maching model quotes.
//      errs: Errors returned by 'correct' and 'correctDates' with format
//               "date: error". If there is no error, the array is empty.
func Merge(model, newQs, oldQs []*T) (qs []*T, errs []error) {
	var oldQs2 []*T
	var control = true
	for _, e := range oldQs {
		if control && e.open < 0 {
			continue
		}
		control = false
		oldQs2 = append(oldQs2, e)
	}
	oldQs = oldQs2

	iqs := 0
	ins := 0
	lns := len(newQs)
	ios := 0
	los := len(oldQs)
	for {
		if iqs >= cts.HistoricQuotes {
			break
		}

		if ins < lns {
			if ios < los {
				nq := newQs[ins]
				oq := oldQs[ios]
				if nq.date > oq.date {
					qs = append(qs, newQs[ins])
					ins++
					iqs++
					continue
				}
				if nq.date < oq.date {
					qs = append(qs, oldQs[ios])
					ios++
					iqs++
					continue
				}
				qs = append(qs, oldQs[ios])
				ins++
				ios++
				iqs++
				continue
			}
			qs = append(qs, newQs[ins])
			ins++
			iqs++
			continue
		}

		if ios < los {
			qs = append(qs, oldQs[ios])
			ios++
			iqs++
			continue
		}

		break
	}

	if len(model) != 0 {
		qs, errs = CorrecDates(model, qs)
	}

	qs, errs2 := Correct(qs)
	errs = append(errs, errs2...)

	sort.Slice(errs, func(i, j int) bool {
		return errs[i].Error() > errs[j].Error()
	})

	return
}

func VolumeAvg(qs []*T) float64 {
	if len(qs) > cts.QuotesVolume {
		qs = qs[:cts.QuotesVolume]
	}
	var vols []float64
	for _, q := range qs {
		mx := q.Max()
		mn := q.Min()
		v := float64(q.Vol())
		if mx >= 0 && mn >= 0 && v >= 0 {
			vols = append(vols, (mx+mn)*v/2)
		}
	}
	sort.Slice(vols, func(i, j int) bool {
		return vols[i] < vols[j]
	})

	var sm float64
	n := 0
	lim := len(vols) / 2
	for i, v := range vols {
		if i >= lim {
			break
		}
		sm += v
		n++
	}
	var av float64
	if n > 0 {
		av = sm / float64(n)
	}
	return av
}
