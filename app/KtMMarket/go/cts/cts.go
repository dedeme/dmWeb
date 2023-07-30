// Copyright 01-Jul-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Global constants
package cts

const (
	// Application name
	AppName = "KtMMarket"

	// Data version
	DataVersion = "KutMarket\nData version: 202207\n"

	// Application directory.
	WebDir = "/dm/wwwcgi/dmcgi/" + AppName

	// Data directory.
	KtMMarketDataDir = "/home/deme/.dmGoApp/KtMMarket/data"

	// KtMarket Data directory.
	KtMarketDataDir = "/home/deme/.dmGoApp/KtMarket/data"

	// Time (seconds) of connection expiration
	Expiration = 900

	// Investors initial capital for each cycle
	InitialCapital = 100000.0

	// Bet
	Bet = 10000.0

	// Minimun cash to bet
	MinToBet = 11000.0

	// No lost multiplicator
	NoLostMultiplicator = 1.05

	// Assets ratio for evaluation.
	AssetsRatio = 0.7

	// Maximum assets to calculate evaluation ratio. (IC € generate IC*MPAR €)
	MaxAssets = InitialCapital * MaxProfitsAvgRatio

	// Profits ratio for evaluation.
	ProfitsAvgRatio = 1 - AssetsRatio

	// Maximum ratio for profits evaluation. (1€ generates 3€)
	MaxProfitsAvgRatio = 3.0

	// Weeks for geometrical results average
	EvalWeeks = 50.0

	// Number of steps for evaluation
	EvalSteps = 20

	// Number for step environmet.
	EnvSteps = 5
)
