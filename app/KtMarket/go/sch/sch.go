// Copyright 03-Jun-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Scheduler
package sch

import (
	"github.com/dedeme/KtMarket/cts"
	"github.com/dedeme/KtMarket/data/activity"
	"github.com/dedeme/KtMarket/data/calendar"
	"github.com/dedeme/KtMarket/data/ixsChartEntry"
	"github.com/dedeme/KtMarket/data/nick"
	"github.com/dedeme/KtMarket/data/profitsEntry"
	"github.com/dedeme/KtMarket/db"
	"github.com/dedeme/KtMarket/db/acc/profitsDb"
	"github.com/dedeme/KtMarket/net"
	"github.com/dedeme/ktlib/arr"
	"github.com/dedeme/ktlib/log"
	"github.com/dedeme/ktlib/sys"
	"github.com/dedeme/ktlib/thread"
	"github.com/dedeme/ktlib/time"
)

var Busy bool

func HistoricUpdate() {
	log.Warning("Historic update")

	var nks []*nick.T
	nks = db.NicksTb().Read().List

	for _, nk := range nks {
		warns, err := net.UpdateHistoric(nk)
		if err != "" {
			log.Error(nk.Name + ":\n" + err)
		} else {
			for _, w := range warns {
				log.Warning(nk.Name + ":\n" + w)
			}
		}
	}

	err := db.UpdateInvestors()
	if err != "" {
		log.Error(err)
	}
	db.Operations()
}

func dailyUpdate() {
	db.UpdateDailyCharts(net.ServerReadDaily, net.ReadIndexes)
	db.UpdateHistoricProfits()
}

func dailyActivate() {
	log.Warning("Daily activiate")
	db.DailyTb().Write(nick.NewTbIdVal(time.ToStr(time.Now()), []*nick.IdValT{}))
	err := db.UpdateInvestors()
	if err != "" {
		log.Error(err)
	}
	db.ActivateDailyCharts(net.ReadIndexes)
	dailyUpdate()
	db.NextServer()
}

func dailyDeactivate() {
	log.Warning("Daily deactiviate")
	err := db.UpdateInvestors()
	if err != "" {
		log.Error(err)
	}
	db.Operations()

	invs := arr.Map(profitsDb.Read(), func(es []*profitsEntry.T) float64 {
		return es[len(es)-1].Total()
	})
	ixs, err := net.ReadIndexes()
	if err != "" {
		log.Error(err)
	}
	db.IndexesTb().Write([]*ixsChartEntry.T{ixsChartEntry.NewNow(invs, ixs)})
}

func Start() {
	for {
		tmSleep := cts.SchedulerSleepTime
		thread.Sync(func() {
			Busy = true

			var cal *calendar.T
			cal = db.CalendarTb().Read()
			now := time.Now()
			if calendar.IsTimeToWatch(cal, now) {
				tmSleep = cts.SchedulerWatchingTime
			}

			confDb := db.ConfTb()
			confTb := confDb.Read()
			act := confTb.Activity
			if time.ToStr(act.Time) != time.ToStr(now) ||
				time.Hour(act.Time) < cts.ActHistoricStart {
				if time.Hour(now) > cts.ActHistoricStart {
					HistoricUpdate()
					act = activity.NewNow(cts.ActSleeping)
					confTb.Activity = act
					confDb.Write(confTb)
				}
			}

			if calendar.IsMarketDay(cal, now) {
				if act.Activity == cts.ActSleeping {
					if calendar.IsOpen(cal, now) {
						dailyActivate()
						act = activity.NewNow(cts.ActActive)
						confTb.Activity = act
						confDb.Write(confTb)
					} else if time.Hour(act.Time) < 12 && time.Hour(now) > 12 {
						dailyActivate()
						dailyDeactivate()
						act = activity.NewNow(cts.ActSleeping)
						confTb.Activity = act
						confDb.Write(confTb)
					}
				} else {
					dailyUpdate()
					if !calendar.IsOpen(cal, now) {
						dailyDeactivate()
						act = activity.NewNow(cts.ActSleeping)
						confTb.Activity = act
						confDb.Write(confTb)
					}
				}
			}

			Busy = false
		})
		sys.Sleep(tmSleep)
	}
}
