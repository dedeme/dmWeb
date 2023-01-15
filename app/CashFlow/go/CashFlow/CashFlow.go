// Copyright 18-Aug-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Main go file
package main

import (
	"github.com/dedeme/CashFlow/data/cts"
	"github.com/dedeme/CashFlow/db"
	"github.com/dedeme/CashFlow/pgs/budgetPage"
	"github.com/dedeme/CashFlow/pgs/budgetPage/budgetEdit"
	"github.com/dedeme/CashFlow/pgs/budgetPage/budgetView"
	"github.com/dedeme/CashFlow/pgs/budgetPage/fixProblem"
	"github.com/dedeme/CashFlow/pgs/changePass"
	"github.com/dedeme/CashFlow/pgs/mainPg"
	"github.com/dedeme/CashFlow/pgs/planPage"
	"github.com/dedeme/CashFlow/pgs/year"
	"github.com/dedeme/ktlib/cgi"
	"github.com/dedeme/ktlib/cryp"
	"github.com/dedeme/ktlib/js"
	"github.com/dedeme/ktlib/str"
	"github.com/dedeme/ktlib/sys"
)

func sourceProcess(ck string, mrq map[string]string) string {
	source := js.Rs(mrq["source"])
	switch source {
	case "Main":
		return mainPg.Process(ck, mrq)
	case "Year":
		return year.Process(ck, mrq)
	case "PlanPage":
		return planPage.Process(ck, mrq)
	case "Budget":
		return budgetPage.Process(ck, mrq)
	case "FixProblem":
		return fixProblem.Process(ck, mrq)
	case "BudgetView":
		return budgetView.Process(ck, mrq)
	case "BudgetEdit":
		return budgetEdit.Process(ck, mrq)
	case "ChangePass":
		return changePass.Process(ck, mrq)
	default:
		panic(str.Fmt("Value of source ('%v') is not valid", source))
	}
}

func main() {
	args := sys.Args()
	if len(args) != 2 {
		sys.Println("CashFlow need one and only one argument.")
		return
	}
	rq := args[1]

	cgi.Initialize(cts.Home, 900)
	db.Initialize()

	ix := str.IndexByte(rq, ':')
	//............................................................... CONNECTION
	if ix == -1 {
		sys.Print(cgi.Connect(rq))
		return
	}

	//........................................................... AUTHENTICATION
	if ix == 0 {
		key := cryp.Key(cts.App, cgi.Klen)
		data := cryp.Decode(key, rq[1:])
		ps := str.Split(data, ":")
		sys.Print(cgi.Authentication(key, ps[0], ps[1], ps[2] == "1"))
		return
	}

	//............................................................... NORMAL DATA
	sessionId := rq[:ix]
	conKey := ""
	rest := rq[ix+1:]
	ix = str.IndexByte(rest, ':')
	if ix != -1 {
		conKey = rest[:ix]
		rest = rest[ix+1:]
	}
	comKey, ok := cgi.GetComKey(sessionId, conKey)
	if !ok {
		sys.Print(cgi.RpExpired())
		return
	}

	j := cryp.Decode(comKey, rest)
	data := js.Ro(j)
	sys.Print(sourceProcess(comKey, data))
}
