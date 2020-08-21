// Copyright 15-Aug-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Global constants
package cts

const (
	//Application ----------------------------------------------------------------
	// Application name
	AppName = "MultiMarket"

	// Data version
	DataVersion = "MultiMarket\nData version: 202008\n"

	// Data directory relative to sys_home()
	DataPath = "data"

	// Log maximum entries
	LogMaxLength = 25000

	///Number of quotes used to calculate companies volume.
	QuotesVolume = 100

	// Server --------------------------------------------------------------------
	// Communications port.
	Port = "50205"

	// Time (seconds) of connection expiration
	Expiration = 900

	// Time (milliseconds) to wait a web server response
	WebWait = 125000

	// Server configuration stopped
	ServerStopped = 0

	// Server configuration active
	ServerActive = 1

	// Server configuration selected
	ServerSelected = 2

	// Scheduler -----------------------------------------------------------------
	// Time to scheduler sleep (milliseconds)
	SchedulerSleep = 2000

	// Tics of schedulerSleep
	SchedulerTimes = 60 // 2 minutes

	// External servers (Infobolsa, Finanzas, etc.) delay in SECONDS
	ServersDelay = 900 // 15 minutes

	// Activity state
	ActSleeping1 = "Sleeping (1)"

	// Activity state
	ActHistoric = "Historic"

	// Activity state
	ActSleeping2 = "Sleeping (2)"

	// Activity state
	ActActivating = "Activating"

	// Activity state
	ActActive = "Active"

	// Activity state
	ActDeactivating = "Deactivating"

	// Hour to start actHistoric fase
	ActHistoricStart = 3

	// Hour to finish actHistoric fase
	ActHistoricEnd = 8

	// Accounting ----------------------------------------------------------------
	// Number of investors
	Managers = 2

	// Fleas ---------------------------------------------------------------------
	// Number of quotes in historic
	HistoricQuotes = 610

	// Fleas initial capital for each cycle
	InitialCapital = 100000.0

	// Bet
	Bet = 10000.0

	// Minimun cash to bet
	MinToBet = 11000.0

	// Minimum operations to survive (multiplicator: days * minSells)
	MinSells = 0.08

	// Maximun operations to survive (multiplicator: days * maxSells)
	MaxSells = 0.16

	// Maximum of mutation
	MutationMultiplier = 0.3

	// Number of cycle per parameter after insertion to finish a process
	Cycles = 5

	// Number of cycle to insert historic results
	InsertionCycle = 10

	// Number of fleas per model
	FleasPerModel = 2000

	// Fleas evaluation ----------------------------------------------------------
	// Historic simulation ratio
	AssetsRatio = 0.3

	// Average of profits ratio
	ProfitsAvgRatio = 0.3

	// Variance of profits ratio
	ProfitsVaRatio = 0.1

	// Flea age ratio
	AgeRatio = 0.2

	// Number of fleas in pool
	PoolNumber = 1000

	// Number of new fleas for adding to pool.
	PoolAddNumber = 10

	// Number of fleas in ranking
	RankingNumber = 40

	// Number of fleas that changes per session
	RankingChanges = 4

	// Number of days of historic rankings.
	RankingDays = 10
)
