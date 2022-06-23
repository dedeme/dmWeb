// Copyright 05-Jun-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Settings-Calendar page
package calendar

import (
	"github.com/dedeme/KtMarket/data/calendar"
	"github.com/dedeme/KtMarket/db"
	"github.com/dedeme/ktlib/arr"
	"github.com/dedeme/ktlib/cgi"
	"github.com/dedeme/ktlib/js"
	"github.com/dedeme/ktlib/thread"
)

func Process(ck string, mrq cgi.T) string {
	rq := js.Rs(mrq["rq"])
	switch rq {
	case "idata":
		var cl string
		thread.Sync(func() {
			cl = db.CalendarTb().ReadJs()
		})
		return cgi.Rp(ck, cgi.T{
			"calendar": cl,
		})
	case "setGeneral":
		thread.Sync(func() {
			cl := db.CalendarTb().Read()
			cl.General = calendar.TimetableFromJs(mrq["timetable"])
			db.CalendarTb().Write(cl)
		})
		return cgi.RpEmpty(ck)
	case "setHolidays":
		thread.Sync(func() {
			cl := db.CalendarTb().Read()
			cl.Holidays = arr.Map(js.Ra(mrq["holidays"]), func(j string) string {
				return js.Rs(j)
			})
			db.CalendarTb().Write(cl)
		})
		return cgi.RpEmpty(ck)
	case "setSpecialDays":
		thread.Sync(func() {
			cl := db.CalendarTb().Read()
			cl.SpecialDays = arr.Map(
				js.Ra(mrq["specialDays"]),
				func(j string) *calendar.MarketDayT {
					return calendar.MarketDayFromJs(j)
				},
			)
			db.CalendarTb().Write(cl)
		})
		return cgi.RpEmpty(ck)
	default:
		panic("Value of rq (" + rq + ") is not valid")
	}
}
