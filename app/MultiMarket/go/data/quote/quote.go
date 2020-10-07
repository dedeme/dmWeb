// Copyright 21-Aug-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Quote data.
package quote

import (
	"fmt"
	"github.com/dedeme/MultiMarket/data/cts"
	"github.com/dedeme/golib/json"
	"sort"
	"strconv"
	"strings"
	"time"
)

// Auxiliar function
func andError(old, new string) string {
	if old != "" {
		return old + " && " + new
	}
	return new
}

type T struct {
	date  string
	open  float64
	close float64
	max   float64
	min   float64
	vol   int
	err   bool
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
//      serr : A message (e.g. Close > Max) if there was some corrected error or
//             an empty string otherwise.
func Correct1(q *T) (newQ *T, serr string) {
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
		serr = "Open > Max"
	}
	if close > max {
		max = close
		serr = andError(serr, "Close > Max")
	}
	if open < min {
		min = open
		serr = andError(serr, "Open < Min")
	}
	if close < min {
		min = close
		serr = andError(serr, "Close < Min")
	}

	if serr != "" {
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
//      serr : A message (e.g. Open > Max) if there was some corrected error or
//             an empty string otherwise.
func Correct2(last, previous *T) (newQ *T, serr string) {
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
		newQ, serr = Correct1(last)
		return
	}

	if open > max {
		if open == open0 && max != max0 {
			open = max
		} else {
			max = open
		}
		serr = "Open > Max"
	}
	if close > max {
		if close == close0 && max != max0 {
			close = max
		} else {
			max = close
		}
		serr = andError(serr, "Close > Max")
	}
	if open < min {
		if open == open0 && min != min0 {
			open = min
		} else {
			min = open
		}
		serr = andError(serr, "Close > Max")
	}
	if close < min {
		if close == close0 && min != min0 {
			close = min
		} else {
			min = close
		}
		serr = andError(serr, "Close < Min")
	}

	if serr != "" {
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
//      serr : A message (e.g. Open > Max) if there was some corrected error or
//             an empty string otherwise.
func Correct3(last, previous *T) (newQ *T, serr string) {
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
		serr = "Open +20%"
	}
	if close > close0*1.2 {
		serr = andError(serr, "Close +20%")
	}
	if max > max0*1.2 {
		serr = andError(serr, "Max +20%")
	}
	if min > min0*1.2 {
		serr = andError(serr, "Min +20%")
	}

	if open < open0*0.8 {
		serr = andError(serr, "Open -20%")
	}
	if close < close0*0.8 {
		serr = andError(serr, "Close -20%")
	}
	if max < max0*0.8 {
		serr = andError(serr, "Max -20%")
	}
	if min < min0*0.8 {
		serr = andError(serr, "Min -20%")
	}

	if serr != "" {
		newQ = &T{date, open, close, max, min, vol, true}
	}
	return
}

// Corrects "qs" and returns a list with corrected quotes.
//    qs: Historic quotes.
//    ----
//    Returns:
//      newQs  : Quotes corrected, if necessary.
//      serrors: A list with messages coming from correct1, correct2 and
//               correct3. If there was no error, it is an empty list.
func Correct(qs []*T) (newQs []*T, serrors []string) {
	if len(qs) == 0 {
		return
	}
	q2 := qs[0]
	for i := 1; i < len(qs); i++ {
		q1 := q2
		q2 = qs[i]
		q, e := Correct2(q1, q2)
		if e != "" {
			serrors = append(serrors, q.date+": "+e)
		}
		q, e = Correct3(q, q2)
		if e != "" {
			serrors = append(serrors, q.date+": "+e)
		}
		newQs = append(newQs, q)
	}
	q, e := Correct1(q2)
	if e != "" {
		serrors = append(serrors, q.date+": "+e)
	}
	q, e = Correct3(q, q2)
	if e != "" {
		serrors = append(serrors, q.date+": "+e)
	}
	newQs = append(newQs, q)

	return
}

// Corrects "qs" and returns a list with corrected quotes.
//    mqs: Historic quotes of company model
//    qs : Historic quotes of company.
//    ----
//    Returns:
//      newQs  : Quotes corrected, if necessary.
//      serrors: Errors by extra o missing quotes. If there was no errors,
//               it is an empty list.
func CorrecDates(mqs, qs []*T) (newQs []*T, serrors []string) {
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
					serrors = append(serrors, mq.date+": "+"Missing quote")
					newQs = append(newQs, &T{mq.date, -1, -1, -1, -1, -1, true})
					mi++
					continue
				}
				if mq.date < q.date {
					serrors = append(serrors, q.date+": "+"Extra quote")
					i++
					continue
				}
				newQs = append(newQs, q)
				i++
				mi++
				continue
			}
			serrors = append(serrors, mq.date+": "+"Missing quote")
			newQs = append(newQs, &T{mq.date, -1, -1, -1, -1, -1, true})
			mi++
			continue
		}
		if i < lqs {
			q := qs[i]
			serrors = append(serrors, q.date+": "+"Extra quote")
			i++
			continue
		}
		break
	}
	return
}

// Unifies serveral list of quotes in one.
//    aQs: Arrays to unify. Every quote has a .err value equals to false.
//    ix : The list of "aQs" selected for tiebreaks.
func Unify(aQs [][]*T, ix int) (r []*T) {
	fsel := func(qs []*T, fn func(q *T) string) string {
		rs := make(map[string]int)
		for i, e := range qs {
			pts := 2
			if i == ix {
				pts++
			}
			k := fn(e)
			v, ok := rs[k]
			if ok {
				rs[k] = v + pts
			} else {
				rs[k] = pts
			}
		}
		max := 0
		var selK *string
		for k, v := range rs {
			if v > max {
				max = v
				selK = &k
			}
		}

		if selK == nil {
			panic("Selk must be not nil")
		}
		return *selK
	}

	laQs := len(aQs)
	var is []int
	var lens []int
	for i := 0; i < laQs; i++ {
		is = append(is, 0)
		lens = append(lens, len(aQs[i]))
	}

	for {
		var qs []*T
		for i, e := range aQs {
			if is[i] < lens[i] {
				qs = append(qs, e[is[i]])
			} else {
				qs = append(qs, nil)
			}
		}

		var maxDate *string
		for _, e := range qs {
			if e != nil {
				if maxDate == nil {
					maxDate = &e.date
				} else if e.date > *maxDate {
					maxDate = &e.date
				}
			}
		}
		if maxDate == nil {
			break
		}

		var qs2 []*T
		cyes := 0
		cnot := 0
		for i, e := range qs {
			if e != nil {
				if e.date == *maxDate {
					qs2 = append(qs2, e)
					is[i]++
					cyes++
				} else {
					cnot++
				}
			}
		}
		if cyes > cnot ||
			(cyes == cnot && qs[ix] != nil && qs[ix].date == *maxDate) {

			sopen := fsel(qs2, func(q *T) string {
				return fmt.Sprintf("%.4f", q.open)
			})
			open, _ := strconv.ParseFloat(sopen, 64)

			sclose := fsel(qs2, func(q *T) string {
				return fmt.Sprintf("%.4f", q.close)
			})
			close, _ := strconv.ParseFloat(sclose, 64)

			smax := fsel(qs2, func(q *T) string {
				return fmt.Sprintf("%.4f", q.max)
			})
			max, _ := strconv.ParseFloat(smax, 64)

			smin := fsel(qs2, func(q *T) string {
				return fmt.Sprintf("%.4f", q.min)
			})
			min, _ := strconv.ParseFloat(smin, 64)

			svol := fsel(qs2, func(q *T) string {
				return fmt.Sprintf("%d", q.vol)
			})
			vol, _ := strconv.Atoi(svol)

			r = append(r, &T{*maxDate, open, close, max, min, vol, false})
		}
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
//      qs     : Array made with the following process:
//               1. Every quote on top with 'open = -1' is removed from "oldQs"
//                  in the dates range of "newQs"
//               2. If there are new and old quotes for the same date, that of
//                  'old' is selected.
//               3. The return array is corrected in the range of 'new' dates
//                  and added or removed quotes maching model quotes.
//      serrors:  Errors returned by 'correct' and 'correctDates' with format
//                "date: error". If there is no error, the array is empty.
func Merge(model, newQs, oldQs []*T) (qs []*T, serrors []string) {
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
		qs, serrors = CorrecDates(model, qs)
	}

	qs, serrors2 := Correct(qs)
	serrors = append(serrors, serrors2...)

	sort.Slice(serrors, func(i, j int) bool { return serrors[i] > serrors[j] })

	return
}
