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
	WebWait = 30000

	// Minimum entries for a valid reading of historic quotes.
	HistoricMinimumEntries = 4

	// Server configuration stopped
	ServerStopped = 0

	// Server configuration active
	ServerActive = 1

	// Server configuration selected
	ServerSelected = 2

	// Net -----------------------------------------------------------------------
	/// Wget user agent configuration.
	/// Necessary for YAHOO.
	/// If it fails, every value can be replaced with ""
	WgetUA1 = "--user-agent"
	WgetUA2 = "Mozilla"
	WgetUA3 = "--load-cookies=" +
		"/home/deme/.mozilla/firefox/bfrqeymk.default/cookies.sqlite"

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
	Investors = 3

	// Maximum of register in Perfomance.tb
	PerformanceMax = 100

	// Fleas ---------------------------------------------------------------------
	// Number of quotes in historic
	HistoricQuotes = 610

	// Fleas initial capital for each cycle
	InitialCapital = 100000.0

	// Bet
	Bet = 10000.0

	// Minimun cash to bet
	MinToBet = 11000.0

	// Fleas evaluation ----------------------------------------------------------
	// Historic simulation ratio
	AssetsRatio = 0.35

	// Maximum historic simulation ratio
	AssetsMax = InitialCapital * 3

	// Average of simulation profits ratio
	ProfitsAvgRatio = 0.65

	// Maximum average of simulation profits ratio
	ProfitsAvgMax = 3

	// Number of days of historic rankings.
	RankingDays = 10

	// Fleas ranges --------------------------------------------------------------
	// Minimum value to analyze
	RangesMin = float64(0.1)
	// Maximum value to analyze (exclusive)
	RangesMax = float64(0.2)
	// Medium value to autamate fleas creation.
	RangesMedium = float64(0.15)
	// Days movil average
	RangesAvg = 250

	// Fleas jump ----------------------------------------------------------------
	// Fleas id to operate separeted by ","
	Jumps           = "JUMP,JUMPT,JUMPTT"
	JumpsMaxRanking = 10
)
