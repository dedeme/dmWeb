// Copyright 03-Jun-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Calendar data.
package calendar

import (
	"github.com/dedeme/KtMarket/cts"
	"github.com/dedeme/ktlib/arr"
	"github.com/dedeme/ktlib/js"
	"github.com/dedeme/ktlib/time"
)

// CALENDAR

type T struct {
	General     *TimetableT
	Holidays    []string
	SpecialDays []*MarketDayT
}

// Constructor.
//   General: General timetable.
//   Holidays: Holidays array.
//   SpecialDays: Days with special timetable.
func New(general *TimetableT, holidays []string, specialDays []*MarketDayT) *T {
	return &T{general, holidays, specialDays}
}

// Returns true if date is a market day.
//
// For example: calendar.IsMarkeDay(cal, time.Now())
//   calendar: Calendar.
//   tm      : Date to test.
func IsMarketDay(cal *T, tm int64) bool {
	wd := time.Weekday(tm)
	d := time.ToStr(tm)
	return wd > 0 && wd < 6 && !arr.Any(cal.Holidays, d)
}

// Returns true if is time to Watch market.
//
// For example: calendar.IsTimetoWatch(cal, time.Now())
//   calendar: Calendar.
//   tm      : Time to test.
func IsTimeToWatch(cal *T, tm int64) bool {
	tm = tm - cts.ServersDelay*1000
	d := time.ToStr(tm)
	if IsMarketDay(cal, tm) {
		o := cal.General.Hopen - 1
		c := cal.General.Hclose + 1

		specialDay, ok := arr.Find(cal.SpecialDays, func(s *MarketDayT) bool {
			return s.Date == d
		})
		if ok {
			o = specialDay.Hopen - 1
			c = specialDay.Hclose + 1
		}

		h := time.Hour(tm)
		return (h >= o && h <= c)
	}
	return false
}

// Returns 'true' if market is open.
//
// For example: calendar.IsOpen(cal, time.Now())
//   calendar: Calendar.
//   tm      : Time to test.
func IsOpen(cal *T, tm int64) bool {
	tm = tm - cts.ServersDelay*1000
	d := time.ToStr(tm)
	if IsMarketDay(cal, tm) {
		hopen := cal.General.Hopen
		mopen := cal.General.Mopen
		hclose := cal.General.Hclose
		mclose := cal.General.Mclose

		specialDay, ok := arr.Find(cal.SpecialDays, func(s *MarketDayT) bool {
			return s.Date == d
		})
		if ok {
			hopen = specialDay.Hopen
			mopen = specialDay.Mopen
			hclose = specialDay.Hclose
			mclose = specialDay.Mclose
		}

		h := time.Hour(tm)
		m := time.Minute(tm)
		return (h > hopen || (h == hopen && m >= mopen)) &&
			(h < hclose || (h == hclose && m <= mclose))
	}
	return false
}

func ToJs(c *T) string {
	return js.Wa([]string{
		TimetableToJs(c.General),
		js.Wa(arr.Map(c.Holidays, func(d string) string {
			return js.Ws(d)
		})),
		js.Wa(arr.Map(c.SpecialDays, func(d *MarketDayT) string {
			return MarketDayToJs(d)
		})),
	})
}

func FromJs(j string) *T {
	a := js.Ra(j)
	return New(
		TimetableFromJs(a[0]),
		arr.Map(js.Ra(a[1]), func(j string) string {
			return js.Rs(j)
		}),
		arr.Map(js.Ra(a[2]), func(j string) *MarketDayT {
			return MarketDayFromJs(j)
		}),
	)
}

// TIMETABLE

type TimetableT struct {
	Hopen  int
	Mopen  int
	Hclose int
	Mclose int
}

func NewTimetable(hopen, mopen, hclose, mclose int) *TimetableT {
	return &TimetableT{hopen, mopen, hclose, mclose}
}

func TimetableToJs(tt *TimetableT) string {
	return js.Wa([]string{
		js.Wi(tt.Hopen),
		js.Wi(tt.Mopen),
		js.Wi(tt.Hclose),
		js.Wi(tt.Mclose),
	})
}

func TimetableFromJs(j string) *TimetableT {
	a := js.Ra(j)
	return NewTimetable(
		js.Ri(a[0]),
		js.Ri(a[1]),
		js.Ri(a[2]),
		js.Ri(a[3]),
	)
}

// MARKETDAY

type MarketDayT struct {
	Date   string
	Hopen  int
	Mopen  int
	Hclose int
	Mclose int
}

func NewMarketDay(date string, hopen, mopen, hclose, mclose int) *MarketDayT {
	return &MarketDayT{date, hopen, mopen, hclose, mclose}
}

func MarketDayToJs(m *MarketDayT) string {
	return js.Wa([]string{
		js.Ws(m.Date),
		js.Wi(m.Hopen),
		js.Wi(m.Hopen),
		js.Wi(m.Mopen),
		js.Wi(m.Hclose),
		js.Wi(m.Mclose),
	})
}

func MarketDayFromJs(j string) *MarketDayT {
	a := js.Ra(j)
	return NewMarketDay(
		js.Rs(a[0]),
		js.Ri(a[1]),
		js.Ri(a[2]),
		js.Ri(a[3]),
		js.Ri(a[4]),
	)
}
