// Copyright 04-Apr-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Schedule controler
package scheduler

import (
	"github.com/dedeme/MMarket/data/cts"
	"github.com/dedeme/MMarket/db"
	"github.com/dedeme/MMarket/db/calendarDb"
	"github.com/dedeme/MMarket/db/log"
	"github.com/dedeme/MMarket/server"
	"github.com/dedeme/golib/date"
	"github.com/dedeme/golib/sys"
)

func historic(ac chan func()) bool {
	log.Info("Scheduler activity: " + cts.ACT_HISTORIC)
	// Read quotes and launch fleas
	return true
}

func sleeping2(ac chan func()) bool {
	log.Info("Scheduler activity: " + cts.ACT_SLEEPING2)

	cont := true
	counter := cts.SCHEDULER_TIMES - 1
	for cont {
		if server.Stoped() {
			return false
		}
		counter++
		if counter >= cts.SCHEDULER_TIMES {
			counter = 0
			db.Sync(func() {
				if calendarDb.IsOpen() {
					cont = false
				}
			})
		}
		sys.Sleep(cts.SCHEDULER_SLEEP)
	}
	return true
}

func activating(ac chan func()) bool {
	log.Info("Scheduler activity: " + cts.ACT_ACTIVATING)
	// Prepare active fase
	return true
}

func active(ac chan func()) bool {
	log.Info("Scheduler activity: " + cts.ACT_ACTIVE)

	cont := true
	counter := cts.SCHEDULER_TIMES - 1
	for cont {
		if server.Stoped() {
			return false
		}
		counter++
		if counter >= cts.SCHEDULER_TIMES {
			counter = 0
			db.Sync(func() {
				// Do daily activities
				if !calendarDb.IsOpen() {
					cont = false
				}
			})
		}
		sys.Sleep(cts.SCHEDULER_SLEEP)
	}

	return true
}

func deactivating(ac chan func()) bool {
	log.Info("Scheduler activity: " + cts.ACT_DEACTIVATING)
	// Prepare sleeping1 fase
	return true
}

func sleeping1(ac chan func()) bool {
	log.Info("Scheduler activity: " + cts.ACT_SLEEPING1)

	cont := true
	counter := cts.SCHEDULER_TIMES - 1
	for cont {
		if server.Stoped() {
			return false
		}
		counter++
		if counter >= cts.SCHEDULER_TIMES {
			counter = 0
			db.Sync(func() {
				hour := date.Now().Hour()
				if hour > cts.ACT_HISTORIC_START && hour < cts.ACT_HISTORIC_END {
					cont = false
				}
			})
		}
		sys.Sleep(cts.SCHEDULER_SLEEP)
	}

	return true
}

func Run(ac chan func(), finalizer chan bool) {
	for {
		if !historic(ac) {
			break
		}
		if !sleeping2(ac) {
			break
		}
		if !activating(ac) {
			break
		}
		if !active(ac) {
			break
		}
		if !deactivating(ac) {
			break
		}
		if !sleeping1(ac) {
			break
		}
	}
	finalizer <- true
}
