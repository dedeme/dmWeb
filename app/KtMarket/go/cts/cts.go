// Copyright 02-Jun-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Global constants
package cts

const (
	//Application ----------------------------------------------------------------
	//
	// Application name
	AppName = "KtMarket"

	// Data version
	DataVersion = "KutMarket\nData version: 202205\n"

	// Application directory.
	HomePath = "/home/deme/.dmGoApp/" + AppName

	// Data directory.
	DataPath = HomePath + "/data"

	//Number of quotes used to calculate companies volume.
	QuotesVolume = 100

	// Server --------------------------------------------------------------------
	//
	// Communications port.
	Port = 8060

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
	SchedulerSleepTime = 900000 // 15'

	// Time to scheduler sleep (milliseconds)
	SchedulerWatchingTime = 120000 // 2'

	// External servers (Infobolsa, Finanzas, etc.) delay in SECONDS
	ServersDelay = 900 // 15 minutes

	// Activity state
	ActSleeping = "Sleeping"

	// Activity state
	ActActive = "Active"

	// Hour to start actHistoric fase
	ActHistoricStart = 2

	// Models - Investors --------------------------------------------------------
	//
	// Number of investors (from 0 to 2)
	Investors = 3

	// Number of quotes in historic
	HistoricQuotes = 610

	// Number of quotes for accounting charts.
	AccountingQuotes = 250

	// Number of quotes for calculate base references
	ReferenceQuotes = AccountingQuotes + 50

	// Fleas initial capital for each cycle
	InitialCapital = 100000.0

	// Bet
	Bet = 10000.0

	// Minimun cash to bet
	MinToBet = 11000.0

	// No lost multiplicator
	NoLostMultiplicator = 1.05

	// Rebuy limit
	RebuyLimit = 400.0
)
