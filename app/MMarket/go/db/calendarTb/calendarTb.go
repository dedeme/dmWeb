// Copyright 13-Apr-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Calendar table.
package calendarTb

import (
	"github.com/dedeme/MMarket/data/cts"
	"github.com/dedeme/MMarket/data/marketDay"
	"github.com/dedeme/MMarket/data/timetable"
	"github.com/dedeme/MMarket/global/sync"
	"github.com/dedeme/golib/date"
	"github.com/dedeme/golib/file"
	"github.com/dedeme/golib/json"
	"path"
)

// Calendar file path.
var fpath string

// General timetable.
var general *timetable.T

// Holiday data in format YYYYMMDD.
var holidays *[]string

// Days with special timetable.
var specialDays *[]*marketDay.T

// Auxiliar function
func write(lk sync.T, tb map[string]json.T) {
	general = timetable.FromJs(tb["general"])

	var h []string
	for _, e := range tb["holidays"].Ra() {
		h = append(h, e.Rs())
	}
	holidays = &h

	var s []*marketDay.T
	for _, e := range tb["specialDays"].Ra() {
		s = append(s, marketDay.FromJs(e))
	}
	specialDays = &s

	file.WriteAll(fpath, json.Wo(tb).String())
}

// Auxiliar function
func read(lk sync.T) map[string]json.T {
	return json.FromString(file.ReadAll(fpath)).Ro()
}

// Initializes calendar table.
//    parent: Parent directory.
func Initialize(lk sync.T, parent string) {
	fpath = path.Join(parent, "Calendar.tb")
	if !file.Exists(fpath) {
		write(lk, map[string]json.T{
			"general":     timetable.New().ToJs(),
			"holidays":    json.Wa([]json.T{}),
			"specialDays": json.Wa([]json.T{}),
		})
		return
	}
	write(lk, read(lk))
}

// Initialize for debug
func Initialize2() {
	general = timetable.New2(9, 0, 17, 40)
	holidays = &[]string{"20210106", "20210402", "20210405", "20211231"}
	specialDays = &[]*marketDay.T{marketDay.New2("20211224", 9, 0, 14, 10)}
}

// Returns general time table 'JSONized'.
//    lk: Synchronization lock.
func General(lk sync.T) json.T {
	return read(lk)["general"]
}

// Modifies general time table.
//    lk: Synchronization lock.
//    js:  'JSONized' Timetable.
func SetGeneral(lk sync.T, js json.T) {
	tb := read(lk)
	tb["general"] = js
	write(lk, tb)
}

// Returns holidays list 'JSONized'.
//    lk: Synchronization lock.
func Holidays(lk sync.T) json.T {
	return read(lk)["holidays"]
}

// Modifies holidays list.
//    lk: Synchronization lock.
//    js:  'JSONized' holidays list.
func SetHolidays(lk sync.T, js json.T) {
	tb := read(lk)
	tb["holidays"] = js
	write(lk, tb)
}

// Returns specialDays list 'JSONized'.
//    lk: Synchronization lock.
func SpecialDays(lk sync.T) json.T {
	return read(lk)["specialDays"]
}

// Modifies specialDays list.
//    lk: Synchronization lock.
//    js:  'JSONized' specialDays list.
func SetSpecialDays(lk sync.T, js json.T) {
	tb := read(lk)
	tb["specialDays"] = js
	write(lk, tb)
}

// Returns 'true' if day is a market day.
//    day: Date to ask.
func IsMarketDay(day date.T) bool {
	wd := day.Weekday()
	if wd == 0 || wd == 6 {
		return false
	}
	ds := day.String()
	for _, d := range *holidays {
		if ds == d {
			return false
		}
	}
	return true
}

// Returns 'true' if day is a market day and its time is before closing.
//    day: Date to ask.
func ToClose(day date.T) bool {
	day = day.AddSeconds(-cts.ServersDelay)
	if IsMarketDay(day) {
		h := day.Hour()
		m := day.Minute()
		ds := day.String()
		hclose := general.Hclose()
		mclose := general.Mclose()
		for _, e := range *specialDays {
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
func IsOpen(day date.T) bool {
	day = day.AddSeconds(-cts.ServersDelay)
	if IsMarketDay(day) {
		h := day.Hour()
		m := day.Minute()
		ds := day.String()
		hopen := general.Hopen()
		mopen := general.Mopen()
		hclose := general.Hclose()
		mclose := general.Mclose()
		for _, e := range *specialDays {
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
func PreviousMarketDay(day date.T) date.T {
	for {
		day = day.Add(-1)
		if IsMarketDay(day) {
			return day
		}
	}
}
