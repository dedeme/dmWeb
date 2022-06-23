// Copyright 03-Jun-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Data base management
package db

import (
	"github.com/dedeme/KtMarket/cts"
	"github.com/dedeme/KtMarket/data/activity"
	"github.com/dedeme/KtMarket/data/calendar"
	"github.com/dedeme/KtMarket/data/conf"
	"github.com/dedeme/KtMarket/data/dailyChart"
	"github.com/dedeme/KtMarket/data/invOperation"
	"github.com/dedeme/KtMarket/data/investor"
	"github.com/dedeme/KtMarket/data/nick"
	"github.com/dedeme/KtMarket/data/quote"
	"github.com/dedeme/KtMarket/data/refBase"
	"github.com/dedeme/KtMarket/data/server"
	"github.com/dedeme/KtMarket/data/serverBox"
	"github.com/dedeme/KtMarket/db/acc/diariesDb"
	"github.com/dedeme/KtMarket/db/acc/profitsDb"
	"github.com/dedeme/ktlib/arr"
	"github.com/dedeme/ktlib/file"
	"github.com/dedeme/ktlib/jstb"
	"github.com/dedeme/ktlib/log"
	"github.com/dedeme/ktlib/path"
	"github.com/dedeme/ktlib/str"
	"github.com/dedeme/ktlib/time"
)

func Initialize() {
	if !file.Exists(cts.DataPath) {
		file.Mkdir(cts.DataPath)
		file.Mkdir(path.Cat(cts.DataPath, "acc"))
		file.Mkdir(path.Cat(cts.DataPath, "daily"))
		file.Mkdir(path.Cat(cts.DataPath, "quotes"))
		file.Write(path.Cat(cts.DataPath, "version.txt"), cts.DataVersion)
	}

	dataVersion := file.Read(path.Cat(cts.DataPath, "version.txt"))
	if dataVersion != cts.DataVersion {
		panic(
			"Application can not continue.\n" +
				"Expected data version:\n" +
				cts.DataVersion +
				"\nBut found:\n" +
				dataVersion)
	}

	diariesDb.Initialize(path.Cat(cts.DataPath, "acc"))
	profitsDb.Initialize(path.Cat(cts.DataPath, "acc"))
}

// Returns 'conf.tb'
func ConfTb() *jstb.T[*conf.T] {
	return jstb.New(
		path.Cat(cts.DataPath, "conf.tb"),
		conf.New("es", activity.New(1652263649990, cts.ActSleeping)),
		conf.ToJs,
		conf.FromJs,
	)
}

// Returns 'nicks.tb'
func NicksTb() *jstb.T[*nick.TbT] {
	return jstb.New(
		path.Cat(cts.DataPath, "nicks.tb"),
		nick.NewTb(0, -1, []*nick.T{}),
		nick.TbToJs,
		nick.TbFromJs,
	)
}

// Returns 'servers.tb'
func ServersTb() *jstb.T[*server.TbT] {
	return jstb.New(
		path.Cat(cts.DataPath, "servers.tb"),
		server.NewTb(),
		server.TbToJs,
		server.TbFromJs,
	)
}

// Returns "calendar.tb"
func CalendarTb() *jstb.T[*calendar.T] {
	return jstb.New(
		path.Cat(cts.DataPath, "calendar.tb"),
		calendar.New(
			calendar.NewTimetable(0, 0, 23, 55),
			[]string{},
			[]*calendar.MarketDayT{},
		),
		calendar.ToJs,
		calendar.FromJs,
	)
}

// Returns "investors.tb"
func InvestorsTb() *jstb.T[*investor.TbT] {
	return jstb.New(
		path.Cat(cts.DataPath, "investors.tb"),
		investor.DefaultTable(),
		investor.TbToJs,
		investor.TbFromJs,
	)
}

// Returns "refBases.tb"
func RefBasesTb() *jstb.T[*refBase.TbT] {
	return jstb.New(
		path.Cat(cts.DataPath, "refBases.tb"),
		refBase.NewTb([]*refBase.T{}),
		refBase.TbToJs,
		refBase.TbFromJs,
	)
}

// Returns "invOperations.tb"
func InvOperationsTb() *jstb.T[*invOperation.TbT] {
	return jstb.New(
		path.Cat(cts.DataPath, "invOperations.tb"),
		invOperation.NewTb([]*invOperation.T{}),
		invOperation.TbToJs,
		invOperation.TbFromJs,
	)
}

// Returns 'daily/daily.tb'
func DailyTb() *jstb.T[*nick.TbIdValT] {
	return jstb.New(
		path.Cat(cts.DataPath, "daily/daily.tb"),
		nick.NewTbIdVal(time.ToStr(time.Now()), []*nick.IdValT{}),
		nick.TbIdValToJs,
		nick.TbIdValFromJs,
	)
}

// Returns "daily/serverBox.tb"
func ServerBoxTb() *jstb.T[*serverBox.T] {
	return jstb.New(
		path.Cat(cts.DataPath, "daily/serverBox.tb"),
		serverBox.New([]string{}),
		serverBox.ToJs,
		serverBox.FromJs,
	)
}

// Returns "daily/dailyChart.tb"
func DailyChartTb() *jstb.T[*dailyChart.TbT] {
	return jstb.New(
		path.Cat(cts.DataPath, "daily/dailyChart.tb"),
		dailyChart.Default(),
		dailyChart.TbToJs,
		dailyChart.TbFromJs,
	)
}

// Quotes tables ---------------------------------------------------------------

func qsPath(nk string) string {
	return path.Cat(cts.DataPath, "quotes", nk+".tb")
}

// Returns 'true' if table 'nk'.tb exists.
//   nk: Nick name of a company (TEF, BBVA, ...).
func QsExists(nk string) bool {
	return file.Exists(qsPath(nk))
}

// Returns quotes of a company.
//
// The order of quotes is from after to before.
//
// Each file line is a quote in string format or an empty string.
//   nk: Nick name of a company (TEF, BBVA, ...).
func QsRead(nk string) (qs []*quote.T, err string) {
	p := qsPath(nk)
	if !file.Exists(p) {
		err = "Quotes file " + nk + ".tb not found."
		return
	}
	qs, err = quote.TxToQs(file.Read(p))
	return
}

// Writes quotes of a company.
//
// It returns 'false' if 'nk' file does not exist.
//   nk: Nick name of a company (TEF, BBVA, ...).
//   qs: quotes.
func QsWrite(nk string, qs []*quote.T) bool {
	p := qsPath(nk)
	if !file.Exists(p) {
		return false
	}
	file.Write(p, arr.Join(arr.Map(qs, func(q *quote.T) string {
		return quote.ToStr(q)
	}), "\n"))
	return true
}

// Modifies a nick name.
//
// If "newName" already exists or "oldName" does not exists, it does nothing.
//    oldName: Name to modify.
//    newName: New name (must be different to oldName).
func QsModifyNick(oldName, newName string) {
	op := qsPath(oldName)
	np := qsPath(newName)
	if file.Exists(op) && !file.Exists(np) {
		file.Rename(op, np)
	}
}

// Remove a table
func QsDelete(nickName string) {
	file.Del(qsPath(nickName))
}

// Checks quotes of a company, writting error and warnings in Log.
//    nickName: Nick source (only for reports)
//    qs: Company quotes.
//    ----
//    Returns:
//      newQqs      : Quotes, corrected if necessary.
//      withWarnings: 'true' if some quotes were corrected.
//      withErrors  : 'true' if an error happend. In this case "qs" = [].
func QsCheck(
	nickName string, qs []*quote.T,
) (newQs []*quote.T, withWarnings, withErrors bool) {

	nksTb := NicksTb().Read()
	mdId := nksTb.Model
	if mdId == -1 {
		log.Error("Nick model has not been set")
		withErrors = true
		return
	}
	md, ok := nksTb.NickFromId(mdId)
	if !ok {
		log.Error(str.Fmt("Nick model with id=%v not found", mdId))
		withErrors = true
		return
	}
	mdQs, err := QsRead(md.Name)
	if err != "" {
		log.Error(err)
		withErrors = true
		return
	}

	newQs, dateErrors := quote.CorrectDates(mdQs, qs)
	for _, e := range dateErrors {
		log.Error(str.Fmt("Error in %v:\n%v", nickName, e))
		withWarnings = true
	}

	newQs, valueErrors := quote.Correct(newQs)
	for _, e := range valueErrors {
		log.Error(str.Fmt("Error in %v:\n%v", nickName, e))
		withWarnings = true
	}

	return
}
