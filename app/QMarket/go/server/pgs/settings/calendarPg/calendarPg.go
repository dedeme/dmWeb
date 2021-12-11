// Copyright 19-Aug-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Settings - calendar page.
package calendarPg

import (
	"fmt"
	"github.com/dedeme/QMarket/data/calendar/marketDay"
	"github.com/dedeme/QMarket/data/calendar/timetable"
	"github.com/dedeme/QMarket/db/calendarTb"
	"github.com/dedeme/QMarket/lock"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/json"
)

func Process(ck string, mrq map[string]json.T) string {
	rq := cgi.RqString(mrq, "rq")
	switch rq {
	case "idata":
		rp := map[string]json.T{}
		lock.Run(func() {
			rp["calendar"] = calendarTb.ReadJs()
		})
		return cgi.Rp(ck, rp)
	case "setGeneral":
		lock.Run(func() {
			c := calendarTb.Read()
			c.General = timetable.FromJs(mrq["timetable"])
			calendarTb.Write(c)
		})
		return cgi.RpEmpty(ck)
	case "setHolidays":
		var holidays []string
		for _, js := range mrq["holidays"].Ra() {
			holidays = append(holidays, js.Rs())
		}
		lock.Run(func() {
			c := calendarTb.Read()
			c.Holidays = holidays
			calendarTb.Write(c)
		})
		return cgi.RpEmpty(ck)
	case "setSpecialDays":
		var specialDays []*marketDay.T
		for _, js := range mrq["specialDays"].Ra() {
			specialDays = append(specialDays, marketDay.FromJs(js))
		}
		lock.Run(func() {
			c := calendarTb.Read()
			c.SpecialDays = specialDays
			calendarTb.Write(c)
		})
		return cgi.RpEmpty(ck)
	default:
		panic(fmt.Sprintf("Value of rq ('%v') is not valid", rq))
	}
}
