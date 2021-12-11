// Copyright 14-Oct-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Global constants
package cts

const (
	//Application ----------------------------------------------------------------
	//
	// Application name
	AppName = "QMarket"

	// Data version
	DataVersion = "QMarket\nData version: 202110\n"

	// Data directory relative to sys_home()
	DataPath = "data"

	// Log maximum entries
	//LogMaxLength = 25000

	///Number of quotes used to calculate companies volume.
	QuotesVolume = 100

	// Server --------------------------------------------------------------------
	//
	// Communications port.
	Port = "55055"

	// Time (seconds) of connection expiration
	Expiration = 900

	// Time (milliseconds) to wait a web server response
	WebWait = 30000

	// External program to read web pages.
	Wget = "Wget"

	// External program to read web pages.
	Puppeteer = "Puppeteer"

	// Minimum entries for a valid reading of historic quotes.
	HistoricMinimumEntries = 4

	// Server configuration stopped
	ServerStopped = 0

	// Server configuration active
	ServerActive = 1

	// Server configuration selected
	ServerSelected = 2

	// Net -----------------------------------------------------------------------
	//
	// Wget user agent configuration.
	// Necessary for YAHOO.
	// If it fails, every value can be replaced with ""
	WgetUA1 = "--user-agent"
	WgetUA2 = "Mozilla"
	WgetUA3 = "--load-cookies=" +
		"/home/deme/.mozilla/firefox/bfrqeymk.default/cookies.sqlite"

	// Scheduler -----------------------------------------------------------------
	//
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

	// Activity state
	ActSleeping3 = "Sleeping (3)"

	// Hour to start actHistoric fase
	ActHistoricStart = 3

	// Hour to finish actHistoric fase
	//ActHistoricEnd = 8

	// Accounting ----------------------------------------------------------------
	//
	// Maximum of register in Perfomance.tb
	PerformanceMax = 100

	// Models --------------------------------------------------------------------
	//
	// Number of models. (Its qlevel numbers are between 0 (inclusive) and
	//   'Qlevels' exclusive)
	Qlevels = 3

	// Number of quotes in historic
	HistoricQuotes = 610

	// Fleas initial capital for each cycle
	InitialCapital = 100000.0

	// Bet
	Bet = 10000.0

	// Minimun cash to bet
	MinToBet = 11000.0

	// Models evaluation ---------------------------------------------------------
	//
	// Historic simulation ratio
	AssetsRatio = 0.35

	// Maximum historic simulation ratio
	AssetsMax = InitialCapital * 3

	// Average of simulation profits ratio
	ProfitsAvgRatio = 0.65

	// Maximum average of simulation profits ratio
	ProfitsAvgMax = 3

	// Days movil average of historic evaluation (data/heval)
	EvalAvgDays = 250

	// Number of entries for ranking
	RankingEntries = 100

	// Number of days of historic rankings.
	RankingDays = 10

	// Parameter ranges ----------------------------------------------------------
	//
	// Minimum value to analyze.
	RangesMin = 12 // id 120000 -> parameter 0.12
	// Group of 10000 to analyze
	RangesGroups = 10 // from 120000 inclusive to 220000 exclusive [0.12-0.22)
	// Number of model per group.
	RangesGroupNumber = 10000
	// Medium value to autamate investors creation.
	ParamIdMedium = 177068
	// Returns the divisor value to convert a model id in a parameter.
	RangesToParam = 1000000 // id = 134567 -> range = id / 1000000 = 0.134567
)
