// Copyright 12-Apr-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package _tests

import (
	"github.com/dedeme/MMarket/data/cts"
	"github.com/dedeme/MMarket/db/calendarTb"
	"github.com/dedeme/golib/date"
	"strings"
	"testing"
)

func TestCalendar(t *testing.T) {
	calendarTb.Initialize2()

	days := []string{"20210106", "20210107", "20210108",
		"20210402", "20210403", "20210404", "20210405", "20210406",
		"20210807", "20210808", "20210809", "20210810",
		"20211224", "20211225", "20211226", "20211227"}
	hours := []int{1, 7, 9, 10, 14, 15, 17, 19}
	minutes := []int{0, 30, 56}

	for _, d := range days {
		dt := date.FromString(d)
		for _, h := range hours {
			dt := dt.AddSeconds(h * 3600)
			for _, m := range minutes {
				dt := dt.AddSeconds(m * 60)

				if strings.Contains(
					"20210106 20210402 20210403 20210404 20210405 20210807 20210808 "+
						"20211225 20211226",
					d) {
					if calendarTb.IsMarketDay(dt) {
						t.Fatal(dt.String() + " is holiday." + fail)
					}
					if calendarTb.ToClose(dt) {
						t.Fatal(dt.String() + " market is closed (holiday)." + fail)
					}
					if calendarTb.IsOpen(dt) {
						t.Fatal(dt.String() + " market is closed (holiday)." + fail)
					}
				}
				if !strings.Contains(
					"20210106 20210402 20210403 20210404 20210405 20210807 20210808 "+
						"20211225 20211226",
					d) {
					if !calendarTb.IsMarketDay(dt) {
						t.Fatal(d + " is market day." + fail)
					}
					if d == "20211224" {
						if h < 14 || (h == 14 && m < 10+cts.ServersDelay/60) {
							if !calendarTb.ToClose(dt) {
								t.Fatal(dt.String() + " market is 'to close'." + fail)
							}
							if h > 9 || h == 9 && m > cts.ServersDelay/60 {
								if !calendarTb.IsOpen(dt) {
									t.Fatal(dt.String() + " market is 'open'." + fail)
								}
							} else {
								if calendarTb.IsOpen(dt) {
									t.Fatal(dt.String() + " market is closed." + fail)
								}
							}
						} else {
							if calendarTb.ToClose(dt) {
								t.Fatal(dt.String() + " market is closed." + fail)
							}
							if calendarTb.IsOpen(dt) {
								t.Fatal(dt.String() + " market is closed." + fail)
							}
						}
					} else {
						if !strings.Contains(
							"20210106 20210402 20210403 20210404 20210405 20210807 20210808 "+
								"20211225 20211226",
							d) {
							if h < 17 || (h == 17 && m < 40+cts.ServersDelay/60) {
								if !calendarTb.ToClose(dt) {
									t.Fatal(dt.String() + " market is 'to close'." + fail)
								}
								if h > 9 || h == 9 && m > cts.ServersDelay/60 {
									if !calendarTb.IsOpen(dt) {
										t.Fatal(dt.String() + " market is 'open'." + fail)
									}
								} else {
									if calendarTb.IsOpen(dt) {
										t.Fatal(dt.String() + " market is closed." + fail)
									}
								}
							} else {
								if calendarTb.ToClose(dt) {
									t.Fatal(dt.String() + " market is closed." + fail)
								}
							}
						}
					}
				}
				prev := calendarTb.PreviousMarketDay(dt).String()
				if strings.Contains("20210106 20210107", d) {
					if r := eqs(prev, "20210105"); r != "" {
						t.Fatal(r)
					}
				}
				if strings.Contains("20210108", d) {
					if r := eqs(prev, "20210107"); r != "" {
						t.Fatal(r)
					}
				}
				if strings.Contains("20210402 20210403 20210404 20210405 20210406", d) {
					if r := eqs(prev, "20210401"); r != "" {
						t.Fatal(r)
					}
				}
				if strings.Contains("202100407", d) {
					if r := eqs(prev, "20210406"); r != "" {
						t.Fatal(r)
					}
				}
				if strings.Contains("20210807 20210808 20210809", d) {
					if r := eqs(prev, "20210806"); r != "" {
						t.Fatal(r)
					}
				}
				if strings.Contains("202100810", d) {
					if r := eqs(prev, "20210809"); r != "" {
						t.Fatal(r)
					}
				}
				if strings.Contains("20211225 20211226 20211227", d) {
					if r := eqs(prev, "20211224"); r != "" {
						t.Fatal(r)
					}
				}
				if strings.Contains("20211224", d) {
					if r := eqs(prev, "20211223"); r != "" {
						t.Fatal(r)
					}
				}

			}
		}
	}
}
