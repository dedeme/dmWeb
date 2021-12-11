// Copyright 14-Oct-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Application scheduler.
package scheduler

import (
	"github.com/dedeme/QMarket/data/acc"
	"github.com/dedeme/QMarket/data/activity"
	"github.com/dedeme/QMarket/data/cts"
	"github.com/dedeme/QMarket/data/dailyChart"
	"github.com/dedeme/QMarket/data/model"
	"github.com/dedeme/QMarket/data/nick"
	"github.com/dedeme/QMarket/data/nickFloat"
	"github.com/dedeme/QMarket/data/performance"
	"github.com/dedeme/QMarket/data/qtable"
	"github.com/dedeme/QMarket/db/acc/diariesDb"
	"github.com/dedeme/QMarket/db/acc/profitsDb"
	"github.com/dedeme/QMarket/db/calendarTb"
	"github.com/dedeme/QMarket/db/confTb"
	"github.com/dedeme/QMarket/db/dailyDb/dailyChartsTb"
	"github.com/dedeme/QMarket/db/dailyDb/dailyTb"
	"github.com/dedeme/QMarket/db/dailyDb/sboxTb"
	"github.com/dedeme/QMarket/db/investorsTb"
	"github.com/dedeme/QMarket/db/logTb"
	"github.com/dedeme/QMarket/db/nicksTb"
	"github.com/dedeme/QMarket/db/performanceTb"
	"github.com/dedeme/QMarket/db/quotesDb"
	"github.com/dedeme/QMarket/db/serversTb"
	"github.com/dedeme/QMarket/lock"
	"github.com/dedeme/QMarket/net"
	"github.com/dedeme/QMarket/scheduler/modelResults"
	"github.com/dedeme/golib/date"
	"time"
)

var act *activity.T

func Initialize() {
	act = confTb.Activity()
}

func activating(errCount int) (cont bool) {
	sboxTb.NextServer()
	sv := sboxTb.GetServer()
	qs, warnings, err := net.ServerReadDaily(sv)
	for err != nil {
		logTb.Error(
			"Fail reading daily server " + sv.ShortName() + ":\n" + err.Error(),
		)
		cont = true
		errCount++
		if errCount > 10 {
			panic("Fail reading daily servers")
		}
		return
	}
	for _, w := range warnings {
		logTb.Info(
			"Information of reading daily server " + sv.ShortName() + ":\n" +
				w.Error(),
		)
	}
	dailyTb.Write(qs)

	hour := date.Now().Hour()
	nicks := nicksTb.Read().SelectedNicks()
	closes := quotesDb.Closes()
	invsTb := investorsTb.Read()

	var nkCls []*nickFloat.T
	for _, nk := range nicks {
		qs, err := quotesDb.Read(nk.Name())
		if err != nil {
			logTb.Error("Faill reading quotes of " + nk.Name() + ":\n" + err.Error())
			continue
		}
		cl := -1.0
		for {
			cl = qs[0].Close()
			qs = qs[1:]
			if cl >= 0 {
				break
			}
		}
		if cl >= 0 {
			nkCls = append(nkCls, nickFloat.New(nk, cl))
		} else {
			logTb.Error("Every quote of " + nk.Name() + " is not valid")
		}
	}

	var pfs [][]*acc.PfEntryT
	for i := 0; i < cts.Qlevels; i++ {
		anns := diariesDb.ReadAnnotations(i)
		_, portfolio, _ := acc.Settlement(anns)
		pfs = append(pfs, portfolio)
	}

	var entries []*dailyChart.T
	for _, e := range nkCls {
		entries = append(
			entries, dailyChart.Initial(closes, pfs, invsTb, hour, qs, e),
		)
	}
	dailyChartsTb.Write(entries)

	act = activity.NewNow(cts.ActActive)
	confTb.SetActivity(act)
	return
}

func updateProfitsHistoric() {
	//dates := append(quotesDb.Dates(), date.Now().String())[1:]
	closes := quotesDb.Closes()
	qs := dailyTb.Read()
	mqs := map[string]float64{}
	for _, q := range qs {
		nk, ok := nicksTb.Read().NickFromId(q.Nick())
		if ok {
			mqs[nk.Name()] = q.Value()
		}
	}
	invsTb := investorsTb.Read()
	for inv := 0; inv < cts.Qlevels; inv++ {
		anns := diariesDb.ReadAnnotations(inv)
		ledger, portfolio, _ := acc.Settlement(anns)
		base := ledger.Cash() + ledger.Capital()
		account := base + ledger.Stocks()
		total := base
		risk := base
		for _, e := range portfolio {
			lastQ, ok := mqs[e.Nick()]
			if ok {
				total += float64(e.Stocks()) * lastQ

				me := model.New(inv, invsTb.Params[e.Nick()][inv])
				cs, ok := closes.NickValuesAdd(e.Nick(), lastQ)
				if ok {
					refs := me.Refs(cs)
					ref := refs[len(refs)-1]
					if ref > lastQ { // Ref exceed
						ref = lastQ
					}
					risk += float64(e.Stocks()) * ref
				} else {
					risk += float64(e.Stocks()) * e.Price()
				}
			} else {
				logTb.Error("Daily quote of " + e.Nick() + " is missing")
				val := float64(e.Stocks()) * e.Price()
				total += val
				risk += val
			}
		}
		profitsDb.Add(inv, total, account, risk)
	}
}

func updateDaily(isFinal bool) {
	mkDailyChart := func(
		closes *qtable.T, hour int, nk *nick.T,
		qs []*nick.QvalueT, oldEntry *dailyChart.T) *dailyChart.T {

		hours := oldEntry.Hours()
		quotes := oldEntry.Quotes()
		missing := true
		for _, q := range qs {
			if q.Nick() == nk.Id() {
				hours = append(hours, hour)
				quotes = append(quotes, q.Value())
				missing = false
				break
			}
		}
		if missing {
			hours = append(hours, hour)
			quotes = append(quotes, quotes[len(quotes)-1])
		}
		return dailyChart.New(
			nk.Name(), oldEntry.Close(), hours, quotes, oldEntry.InvestorsData(),
		)
	}

	sv := sboxTb.GetServer()
	if isFinal {
		servers, sel := serversTb.Read().DailyList()
		sv = servers[sel]
	}

	qvs, warns, err := net.OneServerReadDaily(sv)
	if err != nil {
		qvs, warns, err = net.ServerReadDaily(sv)
	}
	if err != nil {
		logTb.Error("Fail reading daily quotes")
		return
	}
	for _, w := range warns {
		logTb.Info(w.Error())
	}

	dailyTb.Write(qvs)

	hour := date.Now().Hour()
	nicks := nicksTb.Read().SelectedNicks()
	closes := quotesDb.Closes()

	oldEntries := dailyChartsTb.Read()
	var entries []*dailyChart.T
	for _, nk := range nicks {
		var new bool
		for _, e := range oldEntries {
			if e.Nick() == nk.Name() {
				entries = append(entries, mkDailyChart(closes, hour, nk, qvs, e))
				new = false
				break
			}
		}

		if new {
			hqs, err := quotesDb.Read(nk.Name())
			if err != nil {
				logTb.Error("Quotes of " + nk.Name() + " not found:\n" + err.Error())
				continue
			}
			cl := -1.0
			for _, q := range hqs {
				if q.Close() >= 0 {
					cl = q.Close()
					break
				}
			}
			if cl < 0 {
				panic("Every quote of " + nk.Name() + " is not valid")
				continue
			}

			nkCl := nickFloat.New(nk, cl)
			invsTb := investorsTb.Read()
			var pfs [][]*acc.PfEntryT
			for i := 0; i < cts.Qlevels; i++ {
				anns := diariesDb.ReadAnnotations(i)
				_, portfolio, _ := acc.Settlement(anns)
				pfs = append(pfs, portfolio)
			}
			entries = append(
				entries, dailyChart.Initial(closes, pfs, invsTb, hour, qvs, nkCl),
			)
		}

		dailyChartsTb.Write(entries)
	}
}

func updatePerformance() {
	getOpen := func(nick string, date string) (
		open float64, ok bool,
	) {
		qs, err := quotesDb.Read(nick)
		if err != nil {
			return
		}
		for _, q := range qs {
			if q.Date() == date {
				ok = true
				open = q.Open()
				return
			}
		}
		return
	}

	years := diariesDb.Years()
	endYears := 1
	if len(years) > 1 {
		endYears = 2
	}
	perfData := performanceTb.Read()
	lastDate := "000000"
	if len(perfData) == 0 {
		for i := 0; i < endYears; i++ {
			y := years[i]
			allJs := diariesDb.ReadAllJs(y)
			for _, js := range allJs.Ra() {
				ann := acc.AnnotationFromJs(js)
				date := ann.Date()
				_, _, _, ok1 := ann.Operation().Bu()
				_, _, _, ok2 := ann.Operation().Se()
				if (ok1 || ok2) && date > lastDate {
					lastDate = date
				}
			}
		}
		lastDate = date.FromString(lastDate).Add(-1).String()
	} else {
		for _, rc := range perfData {
			if rc.Date() > lastDate {
				lastDate = rc.Date()
			}
		}
	}
	for i := 0; i < endYears; i++ {
		y := years[i]
		allJs := diariesDb.ReadAllJs(y)
		for _, js := range allJs.Ra() {
			ann := acc.AnnotationFromJs(js)
			date := ann.Date()
			if date > lastDate {
				op := ann.Operation()
				if nick, stocks, price, ok := op.Bu(); ok {
					if open, ok := getOpen(nick, date); ok {
						perfData = append(perfData, performance.New(
							nick, date, false, stocks, price, open,
						))
					}
				}
				if nick, stocks, price, ok := op.Se(); ok {
					date := ann.Date()
					if open, ok := getOpen(nick, date); ok {
						perfData = append(perfData, performance.New(
							nick, date, true, stocks, price, open,
						))
					}
				}
			}
		}
	}
	performanceTb.Write(perfData)
}

// Processes -------------------------------------------------------------------

func processSleeping1() {
	lock.Run(func() { logTb.Info("Starts processSleeping1") })

	for {
		if lock.End {
			break
		}

		processEnd := false
		lock.Run(func() {
			if act.Activity() != cts.ActSleeping1 {
				processEnd = true
				return
			}
			if activity.Current(calendarTb.Read()).Activity() != cts.ActSleeping1 {
				act = activity.NewNow(cts.ActHistoric)
				confTb.SetActivity(act)
				processEnd = true
				return
			}
		})
		if processEnd {
			break
		}

		time.Sleep(cts.SchedulerSleep * time.Millisecond)
	}
}

func processHistoric() {
	lock.Run(func() { logTb.Info("Starts processHistoric") })
	if lock.End {
		return
	}

	lock.Run(func() {
		for _, nk := range nicksTb.Read().List() {
			warns, err := net.UpdateHistoric(nk)
			if err != nil {
				logTb.Error(err.Error())
			} else if len(warns) != 0 {
				for _, w := range warns {
					logTb.Info(w.Error())
				}
			}
		}
		updatePerformance()

		investorsTb.Regularize()
	})

	go modelResults.Calculate()

	lock.Run(func() {
		act = activity.NewNow(cts.ActSleeping2)
		confTb.SetActivity(act)
	})
}

func processSleeping2() {
	lock.Run(func() { logTb.Info("Starts processSleeping2") })

	for {
		if lock.End {
			break
		}

		processEnd := false
		lock.Run(func() {
			if act.Activity() != cts.ActSleeping2 {
				processEnd = true
				return
			}
			if activity.Current(calendarTb.Read()).Activity() != cts.ActSleeping2 {
				act = activity.NewNow(cts.ActActivating)
				confTb.SetActivity(act)
				processEnd = true
			}
		})
		if processEnd {
			break
		}

		time.Sleep(cts.SchedulerSleep * time.Millisecond)
	}
}

func processActivating() {
	lock.Run(func() { logTb.Info("Starts processActivating") })
	cont := true
	errCount := 0
	for cont {
		if lock.End {
			return
		}
		lock.Run(func() {
			cont = activating(errCount)
			errCount++
		})

		if lock.End {
			return
		}
		lock.Run(func() {
			updateProfitsHistoric()
		})
	}
}

func processActive() {
	lock.Run(func() { logTb.Info("Starts processActive") })

	count := 0
	for {
		if lock.End {
			return
		}

		processEnd := false
		lock.Run(func() {
			if act.Activity() != cts.ActActive {
				processEnd = true
				return
			}
			if activity.Current(calendarTb.Read()).Activity() != cts.ActActive {
				act = activity.NewNow(cts.ActDeactivating)
				confTb.SetActivity(act)
				processEnd = true
				return
			}

			if count > cts.SchedulerTimes {
				count = 0
				updateDaily(false)
				updateProfitsHistoric()
			}
			count++

		})
		if processEnd {
			break
		}

		time.Sleep(cts.SchedulerSleep * time.Millisecond)
	}
}

func processDeactivating() {
	lock.Run(func() { logTb.Info("Starts processDeactivating") })
	if lock.End {
		return
	}

	lock.Run(func() {
		if act.Activity() != cts.ActDeactivating {
			return
		}

		updateDaily(false)
		updateProfitsHistoric()

		act = activity.NewNow(cts.ActSleeping3)
		confTb.SetActivity(act)
	})
}

func processSleeping3() {
	lock.Run(func() { logTb.Info("Starts processSleeping3") })

	for {
		if lock.End {
			break
		}

		processEnd := false
		lock.Run(func() {
			if act.Activity() != cts.ActSleeping3 {
				processEnd = true
				return
			}
			if activity.Current(calendarTb.Read()).Activity() != cts.ActSleeping3 {
				act = activity.NewNow(cts.ActSleeping1)
				confTb.SetActivity(act)
				processEnd = true
			}
		})
		if processEnd {
			break
		}

		time.Sleep(cts.SchedulerSleep * time.Millisecond)
	}
}

func Start(ch chan int) {
	//  go modelResults.Calculate()

	lock.Run(func() {
		act = act.Update(calendarTb.Read())
		confTb.SetActivity(act)
	})

	for {
		if lock.End {
			ch <- 0
			return
		}

		switch act.Activity() {
		case cts.ActSleeping1:
			processSleeping1()
		case cts.ActHistoric:
			processHistoric()
		case cts.ActSleeping2:
			processSleeping2()
		case cts.ActActivating:
			processActivating()
		case cts.ActActive:
			processActive()
		case cts.ActDeactivating:
			processDeactivating()
		default: // cts.ActSleeping3
			processSleeping3()
		}
	}
}

// Reset the current activity to 'cts.ActActivating' via Sleeping1 -> Historic
// -> Sleeping2 -> Activating
//   --- Must be called in a block 'lock.Run()' ---
// Only works if day is a market one and the current activity is
// cts.ActActive, cts.ActDeactivating or cts.ActSleeping3.
func Reactivate() {
	cal := calendarTb.Read()
	crr := activity.Current(cal)
	a := crr.Activity()
	if cal.IsMarketDay(crr.Date()) &&
		(a == cts.ActActive || a == cts.ActDeactivating || a == cts.ActSleeping3) {
		act = activity.NewNow(cts.ActSleeping1)
		confTb.SetActivity(act)
	}
}
