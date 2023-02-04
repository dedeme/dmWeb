// Copyright 05-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package scheduler

import (
	"github.com/dedeme/MrBackup/data/cts"
	"github.com/dedeme/MrBackup/data/globals"
	"github.com/dedeme/MrBackup/db/log"
	"github.com/dedeme/MrBackup/db/poolDb"
	"github.com/dedeme/ktlib/sys"
	"github.com/dedeme/ktlib/time"
)

// Starts scheduler.
func Start(ch chan int) {
	count := 0
	done := false
	for {
		if count > cts.SchedulerTimes {
			if done {
				if time.Hour(time.Now()) != cts.SchedulerHour {
					done = false
				}
			} else if time.Hour(time.Now()) == cts.SchedulerHour {
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
