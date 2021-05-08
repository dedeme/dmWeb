// Copyright 13-Apr-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Application scheduler.
package scheduler

import (
	//"github.com/dedeme/MMarket/data/acc"
	"github.com/dedeme/MMarket/data/activity"
	"github.com/dedeme/MMarket/data/cts"
	//"github.com/dedeme/MMarket/data/dailyChart"
	//"github.com/dedeme/MMarket/data/manager"
	//"github.com/dedeme/MMarket/data/nick"
	//"github.com/dedeme/MMarket/data/performance"
	//"github.com/dedeme/MMarket/data/qtable"
	//"github.com/dedeme/MMarket/data/server"
	//"github.com/dedeme/MMarket/data/stopper"
	//"github.com/dedeme/MMarket/db/acc/diariesDb"
	//"github.com/dedeme/MMarket/db/acc/profitsDb"
	"github.com/dedeme/MMarket/db/calendarTb"
	//"github.com/dedeme/MMarket/db/conf"
	//"github.com/dedeme/MMarket/db/dailyChartsTb"
	//"github.com/dedeme/MMarket/db/dailyTb"
	//"github.com/dedeme/MMarket/db/log"
	//"github.com/dedeme/MMarket/db/managersTb"
	//"github.com/dedeme/MMarket/db/nicksTb"
	//"github.com/dedeme/MMarket/db/performanceTb"
	//"github.com/dedeme/MMarket/db/quotesDb"
	//"github.com/dedeme/MMarket/db/refsDb"
	//"github.com/dedeme/MMarket/db/sboxTb"
	//"github.com/dedeme/MMarket/db/serversTb"
	//"github.com/dedeme/MMarket/global/fn"
	//"github.com/dedeme/MMarket/global/sync"
	//"github.com/dedeme/MMarket/net"
	//"github.com/dedeme/MMarket/scheduler/fleas"
	"github.com/dedeme/golib/date"
	//"github.com/dedeme/golib/sys"
)

// Returns the initial state of activity depending on where was the last one.
//    lastAct: Activity when server was stopped by last time.
func InitialActivity(lastAct *activity.T) *activity.T {
	return InitialActivity2(lastAct, date.Now())
}

// Equals than 'initialActivity' for debug.
//    lastAct: Activity when server was stopped by last time.
//    today  : current day.
func InitialActivity2(lastAct *activity.T, today date.T) *activity.T {
	day := lastAct.Date()
	ac := lastAct.Activity()
	switch ac {
	case cts.ActSleeping2:
		if day.Eq(today) {
			if calendarTb.IsOpen(today) {
				return activity.New2(today, cts.ActActivating)
			} else if today.Hour() > 12 && calendarTb.IsMarketDay(today) {
				return activity.New2(today, cts.ActDeactivating)
			} else {
				return activity.New2(today, cts.ActSleeping2)
			}
		} else if calendarTb.PreviousMarketDay(today).Df(day) >= 0 {
			return activity.New2(today, cts.ActHistoric)
		} else {
			return activity.New2(today, cts.ActSleeping2)
		}
	case cts.ActSleeping1:
		return activity.New2(today, cts.ActSleeping1)
	case cts.ActActive:
		if day.Eq(today) {
			return activity.New2(today, cts.ActActive)
		} else {
			return activity.New2(today, cts.ActSleeping1)
		}
	case cts.ActHistoric:
		return activity.New2(today, cts.ActHistoric)
	case cts.ActActivating:
		if day.Eq(today) {
			return activity.New2(today, cts.ActActivating)
		} else {
			return activity.New2(today, cts.ActSleeping1)
		}
	case cts.ActDeactivating:
		if day.Eq(today) {
			return activity.New2(today, cts.ActDeactivating)
		} else {
			return activity.New2(today, cts.ActSleeping1)
		}
	default:
		panic("Activity '" + ac + "' is unknown")
	}
}
