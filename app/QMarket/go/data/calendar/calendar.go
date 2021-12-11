// Copyright 16-Oct-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Calendar data.
package calendar

import (
	"github.com/dedeme/QMarket/data/calendar/marketDay"
	"github.com/dedeme/QMarket/data/calendar/timetable"
	"github.com/dedeme/QMarket/data/cts"
	"github.com/dedeme/golib/date"
	"github.com/dedeme/golib/json"
)

type T struct {
	General     *timetable.T
	Holidays    []string
	SpecialDays []*marketDay.T
}

func New() *T {
	return &T{timetable.New(), []string{}, []*marketDay.T{}}
}

// Returns 'true' if day is a market day.
//    day: Date to ask.
func (c *T) IsMarketDay(day date.T) bool {
	wd := day.Weekday()
	if wd == 0 || wd == 6 {
		return false
	}
	ds := day.String()
	for _, d := range c.Holidays {
		if ds == d {
			return false
		}
	}
	return true
}

// Returns 'true' if day is a market day and its time is before closing.
//    day: Date to ask.
func (c *T) IsBeforeClosing(day date.T) bool {
	day = day.AddSeconds(-cts.ServersDelay)
	if c.IsMarketDay(day) {
		h := day.Hour()
		m := day.Minute()
		ds := day.String()
		hclose := c.General.Hclose()
		mclose := c.General.Mclose()
		for _, e := range c.SpecialDays {
			if e.Date() == ds {
				hclose = e.Hclose()
				mclose = e.Mclose()
				break
			}
		}
		return h < hclose || (h == hclose && m <= mclose)
	}
	return false
}

// Returns 'true' if day is a market day and its time is inside the
// corresponding timetable.
//    day: Date to ask.
func (c *T) IsOpen(day date.T) bool {
	day = day.AddSeconds(-cts.ServersDelay)
	if c.IsMarketDay(day) {
		h := day.Hour()
		m := day.Minute()
		ds := day.String()
		hopen := c.General.Hopen()
		mopen := c.General.Mopen()
		hclose := c.General.Hclose()
		mclose := c.General.Mclose()
		for _, e := range c.SpecialDays {
			if e.Date() == ds {
				hopen = e.Hopen()
				mopen = e.Mopen()
				hclose = e.Hclose()
				mclose = e.Mclose()
				break
			}
		}
		return (h > hopen || (h == hopen && m > mopen)) &&
			(h < hclose || (h == hclose && m <= mclose))
	}
	return false
}

// Returns the date of the previous market day. (First day to search is
// today - 1).
//    day: Date to ask.
func (c *T) PreviousMarketDay(day date.T) date.T {
	for {
		day = day.Add(-1)
		if c.IsMarketDay(day) {
			return day
		}
	}
}

func (c *T) ToJs() json.T {
	var holidaysJs []json.T
	for _, h := range c.Holidays {
		holidaysJs = append(holidaysJs, json.Ws(h))
	}
	var specialDaysJs []json.T
	for _, s := range c.SpecialDays {
		specialDaysJs = append(specialDaysJs, s.ToJs())
	}
	return json.Wa([]json.T{
		c.General.ToJs(),
		json.Wa(holidaysJs),
		json.Wa(specialDaysJs),
	})
}

func FromJs(js json.T) *T {
	a := js.Ra()
	var holidays []string
	for _, js := range a[1].Ra() {
		holidays = append(holidays, js.Rs())
	}
	var specialDays []*marketDay.T
	for _, js := range a[2].Ra() {
		specialDays = append(specialDays, marketDay.FromJs(js))
	}
	return &T{
		timetable.FromJs(a[0]),
		holidays,
		specialDays,
	}
}
