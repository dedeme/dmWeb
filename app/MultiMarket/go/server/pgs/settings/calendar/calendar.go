// Copyright 19-Aug-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Settings - calendar page.
package calendar

import (
	"fmt"
	"github.com/dedeme/MultiMarket/db/calendarTb"
	"github.com/dedeme/MultiMarket/global/sync"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/json"
)

func Process(ck string, mrq map[string]json.T) string {
	rq := cgi.RqString(mrq, "rq")
	switch rq {
	case "idata":
		rp := map[string]json.T{}
		sync.Run(func(lk sync.T) {
			rp["general"] = calendarTb.General(lk)
			rp["holidays"] = calendarTb.Holidays(lk)
			rp["specialDays"] = calendarTb.SpecialDays(lk)
		})
		return cgi.Rp(ck, rp)
	case "setGeneral":
		sync.Run(func(lk sync.T) {
			calendarTb.SetGeneral(lk, mrq["timetable"])
		})
		return cgi.RpEmpty(ck)
	case "setHolidays":
		sync.Run(func(lk sync.T) {
			calendarTb.SetHolidays(lk, mrq["holidays"])
		})
		return cgi.RpEmpty(ck)
	case "setSpecialDays":
		sync.Run(func(lk sync.T) {
			calendarTb.SetSpecialDays(lk, mrq["specialDays"])
		})
		return cgi.RpEmpty(ck)
	default:
		panic(fmt.Sprintf("Value of rq ('%v') is not valid", rq))
	}
}
