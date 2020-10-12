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
		if count > 5 { //cts.SchedulerTimes {
			if done {
				if date.Now().Hour() != 3 {
					done = false
				}
			} else if date.Now().Hour() == 3 {
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
