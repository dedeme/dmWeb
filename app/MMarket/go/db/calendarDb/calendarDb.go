// Copyright 05-Apr-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Calendar data base.
package calendarDb

import (
	"github.com/dedeme/MMarket/data/cts"
	"github.com/dedeme/MMarket/data/marketDay"
	"github.com/dedeme/MMarket/data/timeTable"
	"github.com/dedeme/golib/date"
	"github.com/dedeme/golib/file"
	"github.com/dedeme/golib/json"
	gpath "path"
)

var path string

// Initializes conf.db.
//    dir: Parent directory.
func Initialize(dir string) {
	path = gpath.Join(dir, "calendar.db")
	if !file.Exists(path) {
		db := map[string]json.T{
			"general":     timeTable.New().ToJs(),
			"holidays":    json.Wa([]json.T{}),
			"specialDays": json.Wa([]json.T{}),
		}
		write(db)
	}
}

func write(db map[string]json.T) {
	file.WriteAll(path, json.Wo(db).String())
}

func read() map[string]json.T {
	return json.FromString(file.ReadAll(path)).Ro()
}

func General() json.T {
	return read()["general"]
}

func general() *timeTable.T {
	return timeTable.FromJs(General())
}

func SetGeneral(js json.T) {
	cl := read()
	cl["general"] = js
	write(cl)
}

func Holidays() json.T {
	return read()["holidays"]
}

func holidays() (r []string) {
	hjs := Holidays().Ra()
	for _, h := range hjs {
		r = append(r, h.Rs())
	}
	return
}

func SetHolidays(js json.T) {
	db := read()
	db["holidays"] = js
	write(db)
}

func SpecialDays() json.T {
	return read()["specialDays"]
}

func specialDays() (r []*marketDay.T) {
	hjs := SpecialDays().Ra()
	for _, h := range hjs {
		r = append(r, marketDay.FromJs(h))
	}
	return
}

func SetSpecialDays(js json.T) {
	db := read()
	db["specialDays"] = js
	write(db)
}

func IsOpen() (rs bool) {
	time := date.Now().AddSeconds(cts.SERVERS_DELAY)
	weekday := time.Weekday()
	if weekday > 0 && weekday < 6 {
		stime := time.String()
		for _, t := range holidays() {
			if t == stime {
				return
			}
		}
		h := time.Hour()
		m := time.Minute()

		tt := general()
		hopen := tt.Hopen()
		mopen := tt.Mopen()
		hclose := tt.Hclose()
		mclose := tt.Mclose()

		for _, sd := range specialDays() {
			if sd.Date() == stime {
				hopen = sd.Hopen()
				mopen = sd.Mopen()
				hclose = sd.Hclose()
				mclose = sd.Mclose()
				break
			}
		}

		if (h > hopen || (h == hopen && m > mopen)) &&
			(h < hclose || (h == hclose && m < mclose)) {
			return true
		}
	}

	return
}
