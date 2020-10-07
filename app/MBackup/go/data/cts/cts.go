// Copyright 05-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Global constants
package cts

const (
	//Application ----------------------------------------------------------------
	// Application name
	AppName = "MrBackup"
	// Tmp application name
	AppName2 = "MBackup"

	// Data version
	DataVersion = "MrBackup\nData version: 202010\n"

	// Data directory relative to sys_home()
	DataPath = "data"

	// Log length maximum
	LogMaxLength = 10000

	// Server --------------------------------------------------------------------
	// Communications port.
	Port = "50303" // Change to 50203

	// Time (seconds) of connection expiration
	Expiration = 900

	// Time (milliseconds) to wait a web server response
	WebWait = 30000

	// Scheduler -----------------------------------------------------------------
	// Time to scheduler sleep (milliseconds)
	SchedulerSleep = 2000

	// Tics of schedulerSleep
	SchedulerTimes = 18000 // 1 hour
)

// Pools
// First pool is the pool base.
var MrBackupTargets = [3]string{
	"/home/deme/MrBackup",
	"/media/disk1/dm/MrBackup",
	"/media/deme/Elements SE/MrBackup",
}
