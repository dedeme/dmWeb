// Copyright 05-Jun-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Quote data
package quote

import (
	"github.com/dedeme/KtMarket/cts"
	"github.com/dedeme/ktlib/arr"
	"github.com/dedeme/ktlib/js"
	"github.com/dedeme/ktlib/math"
	"github.com/dedeme/ktlib/str"
	"github.com/dedeme/ktlib/time"
)

type T struct {
	Date  string
	Open  float64
	Close float64
	Max   float64
	Min   float64
	Vol   int
	Err   bool
}

// Create a new quote.
//   date : Date of quote in format "YYYYMMDD".
//   open : Open value.
//   close: Close value.
//   max  : Maximum value.
//   min  : Minimum value.
//   vol  : Valume value.
//   err  : 'true' if quote was modified manually.
func New(date string, open, close, max, min float64, vol int, err bool) *T {
	return &T{date, open, close, max, min, vol, err}
}

func ToJs(q *T) string {
	return js.Wa([]string{
		js.Ws(q.Date),
		js.Wd(q.Open),
		js.Wd(q.Close),
		js.Wd(q.Max),
		js.Wd(q.Min),
		js.Wi(q.Vol),
		js.Wb(q.Err),
	})
}

func FromJs(j string) *T {
	a := js.Ra(j)
	return New(
		js.Rs(a[0]),
		js.Rd(a[1]),
		js.Rd(a[2]),
		js.Rd(a[3]),
		js.Rd(a[4]),
		js.Ri(a[5]),
		js.Rb(a[6]),
	)
}

// Returns a string representation of "q".
func ToStr(q *T) string {
	e := "false"
	if q.Err {
		e = "true"
	}
	return q.Date + ":" +
		str.Fmt("%.4f", q.Open) + ":" +
		str.Fmt("%.4f", q.Close) + ":" +
		str.Fmt("%.4f", q.Max) + ":" +
		str.Fmt("%.4f", q.Min) + ":" +
		str.Fmt("%d", q.Vol) + ":" +
		e
}

/// Returns a Quote from its string representation.
func FromStr(s string) (q *T, ok bool) {
	defer func() {
		recover()
	}()

	a := str.SplitTrim(s, ":")
	if len(a) != 7 {
		return
	}

	if a[6] != "true" && a[6] != "false" {
		return
	}
	e := true
	if a[6] == "false" {
		e = false
	} else if a[6] != "true" {
		return
	}

	q = New(
		time.ToStr(time.FromStr(a[0])),
		math.FromStr(a[1]),
		math.FromStr(a[2]),
		math.FromStr(a[3]),
		math.FromStr(a[4]),
		math.ToInt(a[5]),
		e,
	)
	ok = true
	return
}

// Returns quotes, from after to before, from a table type text.
//
// If the final number of quotes is different to 'cts.HistoricQuotes', an
// error is raised.
func TxToQs(tx string) (qs []*T, err string) {
	for _, l := range str.SplitTrim(tx, "\n") {
		q, ok := FromStr(l)
		if !ok {
			err = "Wrong quote: '" + l + "'"
			return
		}
		qs = append(qs, q)
	}
	if len(qs) != cts.HistoricQuotes {
		err = str.Fmt(
			"Wrong quotes number (%d != %d).",
			len(qs), cts.HistoricQuotes,
		)
	}
	return
}

// Returns the number of manual corrections in qs.
//    qs: Quotes
func Manuals(qs []*T) (n int) {
	for _, e := range qs {
		if e.Err {
			n++
		}
	}
	return
}

// Checks maximum and minimum and returns a new quote corrected, if it is
// necessary.
//
// If q.Err = true, quote will not be corrected.
//
// If quote was corrected, its 'err' field is set to 'true'.
//
//    q: Quote to correct.
//    ----
//    Returns:
//      newQ : Corrected quote.
//      errs : List of corrections (e.g. Close > Max).
func Correct1(q *T) (newQ *T, errs []string) {
	newQ = q
	if q.Err {
		return
	}
	date := q.Date
	open := q.Open
	close := q.Close
	max := q.Max
	min := q.Min
	vol := q.Vol

	if open > max {
		max = open
		errs = append(errs, "Open > Max")
	}
	if close > max {
		max = close
		errs = append(errs, "Close > Max")
	}
	if open < min {
		min = open
		errs = append(errs, "Open < Min")
	}
	if close < min {
		min = close
		errs = append(errs, "Close < Min")
	}

	if len(errs) > 0 {
		newQ = &T{date, open, close, max, min, vol, true}
	}
	return
}

// Checks maximum and minimum and returns a new quote corrected, if it is
// necessary.
//
// If q.Err = true, quote will not be corrected.
//
// If quote was corrected, its 'err' field is set to 'true'.
//
//    last    : Quote to correct.
//    previous: Quote previous to "last"
//    ----
//    Returns:
//      newQ : Corrected quote.
//      errs : List of corrections (e.g. Close > Max).
func Correct2(last, previous *T) (newQ *T, errs []string) {
	newQ = last
	if last.Err {
		return
	}
	date := last.Date
	open := last.Open
	close := last.Close
	max := last.Max
	min := last.Min
	vol := last.Vol

	open0 := previous.Open
	close0 := previous.Close
	max0 := previous.Max
	min0 := previous.Min

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
		errs = append(errs, "Open > Max")
	}
	if close > max {
		if close == close0 && max != max0 {
			close = max
		} else {
			max = close
		}
		errs = append(errs, "Close > Max")
	}
	if open < min {
		if open == open0 && min != min0 {
			open = min
		} else {
			min = open
		}
		errs = append(errs, "Open < Min")
	}
	if close < min {
		if close == close0 && min != min0 {
			close = min
		} else {
			min = close
		}
		errs = append(errs, "Close < Min")
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
func Correct3(last, previous *T) (newQ *T, errs []string) {
	newQ = last
	if last.Err {
		return
	}
	date := last.Date
	open := last.Open
	close := last.Close
	max := last.Max
	min := last.Min
	vol := last.Vol

	open0 := previous.Open
	close0 := previous.Close
	max0 := previous.Max
	min0 := previous.Min

	if open0 < 0 {
		return
	}

	if open > open0*1.2 {
		errs = append(errs, "Open +20%")
	}
	if close > close0*1.2 {
		errs = append(errs, "Close +20%")
	}
	if max > max0*1.2 {
		errs = append(errs, "Max +20%")
	}
	if min > min0*1.2 {
		errs = append(errs, "Min +20%")
	}

	if open < open0*0.8 {
		errs = append(errs, "Open -20%")
	}
	if close < close0*0.8 {
		errs = append(errs, "Close -20%")
	}
	if max < max0*0.8 {
		errs = append(errs, "Max -20%")
	}
	if min < min0*0.8 {
		errs = append(errs, "Min -20%")
	}

	if len(errs) > 0 {
		newQ = &T{date, open, close, max, min, vol, true}
	}
	return
}

// Corrects "qs" and returns a list with corrected quotes. Everey slice of quotes
// is ordered from after to before.
//    qs: Historic quotes.
//    ----
//    Returns:
//      newQs  : Quotes corrected, if necessary.
//      errs: A list with errors coming from correct1, correct2 and
//            correct3. If there was no error, it is an empty list.
func Correct(qs []*T) (newQs []*T, errs []string) {
	mkErrs := func(q *T, es []string) []string {
		var r []string
		for _, e := range es {
			r = append(r, q.Date+": "+e)
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

// Corrects "qs" and returns a list with corrected quotes. Everey slice of quotes
// is ordered from after to before.
//    mqs: Historic quotes of company model
//    qs : Historic quotes of company.
//    ----
//    Returns:
//      newQs  : Quotes corrected, if necessary.
//      errs: Errors by extra o missing quotes. If there was no errors,
//               it is an empty list.
func CorrectDates(mqs, qs []*T) (newQs []*T, errs []string) {
	mi := 0
	lmqs := len(mqs)
	i := 0
	lqs := len(qs)
	for {
		if mi < lmqs {
			mq := mqs[mi]
			if i < lqs {
				q := qs[i]
				if mq.Date > q.Date {
					errs = append(errs, mq.Date+": "+"Missing quote")
					newQs = append(newQs, &T{mq.Date, -1, -1, -1, -1, -1, true})
					mi++
					continue
				}
				if mq.Date < q.Date {
					errs = append(errs, q.Date+": "+"Extra quote")
					i++
					continue
				}
				newQs = append(newQs, q)
				i++
				mi++
				continue
			}
			errs = append(errs, mq.Date+": "+"Missing quote")
			newQs = append(newQs, &T{mq.Date, -1, -1, -1, -1, -1, true})
			mi++
			continue
		}
		if i < lqs {
			q := qs[i]
			errs = append(errs, q.Date+": "+"Extra quote")
			i++
			continue
		}
		break
	}
	return
}

// Merges new quotes with others already existent. Everey slice of quotes
// is ordered from after to before.
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
func Merge(model, newQs, oldQs []*T) (qs []*T, errs []string) {
	var oldQs2 []*T
	var control = true
	for _, e := range oldQs {
		if control && e.Open < 0 {
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
				if nq.Date > oq.Date {
					qs = append(qs, newQs[ins])
					ins++
					iqs++
					continue
				}
				if nq.Date < oq.Date {
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
		qs, errs = CorrectDates(model, qs)
	}

	qs, errs2 := Correct(qs)
	errs = append(errs, errs2...)

	arr.Sort(errs, func(e1, e2 string) bool {
		return e1 > e2
	})

	return
}

// Returns the volume average of a company quotes.
//   qs: Company quotes sorted from after to before.
/// \[<Quote>...] -> f
func VolumeAvg(qs []*T) float64 {
	qs = arr.Take(qs, cts.QuotesVolume)

	var vols []float64
	for _, q := range qs {
		mx := q.Max
		mn := q.Min
		v := float64(q.Vol)
		if mx >= 0.0 && mn >= 0.0 && v >= 0.0 {
			vols = append(vols, (mx+mn)*v/2.0)
		}
	}
	arr.Sort(vols, func(v1, v2 float64) bool {
		return v1 < v2
	})

	lim := len(vols) / 2
	return arr.Reduce(
		arr.Take(vols, lim),
		0.0,
		func(r, e float64) float64 {
			return r + e
		}) / float64(lim)
}

// Returns opens-closes from quotes, ordered from before to after.
//    qs: Company quotes sorted from after to before.
func OpenCloses(qs []*T) (opens, closes []float64) {
	lg := len(qs)
	opens = make([]float64, lg)
	closes = make([]float64, lg)
	for i, q := range arr.Reverse(qs) {
		opens[i] = q.Open
		closes[i] = q.Close
	}
	return
}

// Returns the last value geater or equals to 0.0 of values.
//    values: Float values intended for closes and references.
func LastValue(values []float64) float64 {
	for i := len(values) - 1; i >= 0; i-- {
		if values[i] != -1.0 {
			return values[i]
		}
	}
	panic("All values of slice are less than 0.0")
}

// Returns the last two value geater or equals to 0.0 of values.
//    values: Float values intended for closes and references.
func LastValue2(values []float64) (last, penultimate float64) {
	i := len(values) - 1
	for ; i >= 0; i-- {
		if values[i] != -1.0 {
			last = values[i]
			break
		}
	}
	i--
	for ; i >= 0; i-- {
		if values[i] != -1.0 {
			penultimate = values[i]
			return
		}
	}
	panic("All values of slice are less than 0.0")
}
