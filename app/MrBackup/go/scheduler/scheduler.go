// Copyright 05-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package scheduler

import (
	"github.com/dedeme/MrBackup/data/cts"
	"github.com/dedeme/MrBackup/data/globals"
	"github.com/dedeme/MrBackup/db/log"
	"github.com/dedeme/MrBackup/db/poolDb"
	"github.com/dedeme/golib/date"
	"github.com/dedeme/golib/sys"
)

// Starts scheduler.
func Start(ch chan int) {
	count := 0
	done := false
	for {
		if count > cts.SchedulerTimes {
			if done {
				if date.Now().Hour() != cts.SchedulerHour {
					done = false
				}
			} else if date.Now().Hour() == cts.SchedulerHour {
				for globals.IsBusy {
					sys.Sleep(cts.SchedulerSleep)
				}
				globals.IsBusy = true
				log.Info("Starting scheduled backups...")
				poolDb.Update()
				globals.IsBusy = false
				done = true
				log.Info("Scheduled backups finished")
			}
			count = 0
			continue
		}
		count++
		sys.Sleep(cts.SchedulerSleep)
	}
	ch <- 0
}
