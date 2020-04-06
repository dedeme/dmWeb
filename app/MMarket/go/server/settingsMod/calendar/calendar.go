// Copyright 05-Apr-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Settings source hub.
package calendar

import (
	"fmt"
	"github.com/dedeme/MMarket/db"
	"github.com/dedeme/MMarket/db/calendarDb"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/json"
)

func Process(ac chan func(), ck string, mrq map[string]json.T) string {
	key := cgi.RqString(mrq, "rq")
	switch key {
	case "idata":
		rp := map[string]json.T{}
		db.Sync(
			func() {
				rp["general"] = calendarDb.General()
				rp["holidays"] = calendarDb.Holidays()
				rp["specialDays"] = calendarDb.SpecialDays()
			})
		return cgi.Rp(ck, rp)
	case "setGeneral":
		db.Sync(func() {
			calendarDb.SetGeneral(mrq["timeTable"])
		})
		return cgi.RpEmpty(ck)
	case "setHolidays":
		db.Sync(func() {
			calendarDb.SetHolidays(mrq["holidays"])
		})
		return cgi.RpEmpty(ck)
	case "setSpecialDays":
		db.Sync(func() {
			calendarDb.SetSpecialDays(mrq["specialDays"])
		})
		return cgi.RpEmpty(ck)
	default:
		panic(fmt.Sprintf("Value of key is '%v', but it must be '%v'",
			key, "idata | setGeneral | setHolidays | setSpecialDays"))
	}
}
