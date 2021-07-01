// Copyright 08-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Application scheduler.
package scheduler

import (
	"github.com/dedeme/MultiMarket/data/acc"
	"github.com/dedeme/MultiMarket/data/activity"
	"github.com/dedeme/MultiMarket/data/cts"
	"github.com/dedeme/MultiMarket/data/dailyChart"
	"github.com/dedeme/MultiMarket/data/manager"
	"github.com/dedeme/MultiMarket/data/nick"
	"github.com/dedeme/MultiMarket/data/performance"
	"github.com/dedeme/MultiMarket/data/qtable"
	"github.com/dedeme/MultiMarket/data/server"
	"github.com/dedeme/MultiMarket/data/stopper"
	"github.com/dedeme/MultiMarket/db/acc/diariesDb"
	"github.com/dedeme/MultiMarket/db/acc/profitsDb"
	"github.com/dedeme/MultiMarket/db/calendarTb"
	"github.com/dedeme/MultiMarket/db/conf"
	"github.com/dedeme/MultiMarket/db/dailyChartsTb"
	"github.com/dedeme/MultiMarket/db/dailyTb"
	"github.com/dedeme/MultiMarket/db/log"
	"github.com/dedeme/MultiMarket/db/managersTb"
	"github.com/dedeme/MultiMarket/db/nicksTb"
	"github.com/dedeme/MultiMarket/db/performanceTb"
	"github.com/dedeme/MultiMarket/db/quotesDb"
	"github.com/dedeme/MultiMarket/db/refsDb"
	"github.com/dedeme/MultiMarket/db/sboxTb"
	"github.com/dedeme/MultiMarket/db/serversTb"
	"github.com/dedeme/MultiMarket/global/fn"
	"github.com/dedeme/MultiMarket/global/sync"
	"github.com/dedeme/MultiMarket/net"
	"github.com/dedeme/MultiMarket/scheduler/fleas"
	"github.com/dedeme/golib/date"
	"github.com/dedeme/golib/sys"
)

type nkClT struct {
	nk *nick.T
	cl float64
}
type pfMgT struct {
	pf []*acc.PfEntryT
	mg *manager.T
}

func mkDailyChartInit(
	lk sync.T, closes *qtable.T, hour int, nkCl *nkClT,
	qs []*nick.QvalueT, pfMgs []*pfMgT,
) *dailyChart.T {
	nk := nkCl.nk
	nickName := nk.Name()
	q := nkCl.cl
	for _, e := range qs {
		if e.Nick == nk.Id() {
			q = e.Value
		}
	}
	hours := []int{hour, hour}
	quotes := []float64{nkCl.cl, q}
	dates := quotesDb.Dates(lk)

	var managersData []*dailyChart.DataT
	for _, e := range pfMgs {
		stocks := 0
		price := 0.0
		for _, pfe := range e.pf {
			if pfe.Nick() == nickName {
				stocks = pfe.Stocks()
				price = pfe.Price()
			}
		}
		man := e.mg
		mdPars, ok := man.Nicks()[nickName]
		if !ok {
			mdPars = man.Base
		}
		ref := nkCl.cl
		cls, ok := closes.NickValues(nickName)
		if ok {
			refs := refsDb.MkRefs(
				lk, nickName, dates, cls, mdPars.Model(), mdPars.Params(),
			)
			ref = refs[len(refs)-1]
		}

		managersData = append(
			managersData, dailyChart.DataNew(stocks, price, ref),
		)
	}
	return dailyChart.New(nickName, nkCl.cl, hours, quotes, managersData)
}

func activating() {
	cont := true
	for cont {
		sync.Run(func(lk sync.T) {
			cont = false
			sboxTb.NextServer(lk)
			sv := sboxTb.GetServer(lk)
			qs, ok := net.ServerReadDaily(lk, sv)
			if !ok {
				log.Error(lk, "Fail reading daily server "+sv.ShortName())
				cont = true
				return
			}

			hour := date.Now().Hour()
			dailyTb.Write(lk, qs)
			nicks := nicksTb.SelectedNicks(lk)
			closes := quotesDb.Closes(lk)

			var nkCls []*nkClT
			for _, nk := range nicks {
				qs := quotesDb.Read(lk, nk.Name())
				if len(qs) == 0 {
					log.Error(lk, "Quotes of "+nk.Name()+" not found")
					continue
				}
				cl := -1.0
				for {
					if len(qs) == 0 {
						break
					}
					cl = qs[0].Close()
					qs = qs[1:]
					if cl >= 0 {
						break
					}
				}
				if cl >= 0 {
					nkCls = append(nkCls, &nkClT{nk, cl})
				} else {
					log.Error(lk, "Every quote of "+nk.Name()+" is not valid")
				}
			}

			managers := managersTb.Read(lk)
			var pfMgs []*pfMgT
			for i := 0; i < len(managers); i++ {
				anns := diariesDb.ReadAnnotations(lk, i)
				_, portfolio, _ := acc.Settlement(anns)
				pfMgs = append(pfMgs, &pfMgT{portfolio, managers[i]})
			}

			var entries []*dailyChart.T
			for _, e := range nkCls {
				entries = append(
					entries, mkDailyChartInit(lk, closes, hour, e, qs, pfMgs),
				)
			}
			dailyChartsTb.Write(lk, entries)
		})
	}
}

func updateProfitsHistoric() {
	sync.Run(func(lk sync.T) {
		dates := append(quotesDb.Dates(lk), date.Now().String())[1:]
		closes := quotesDb.Closes(lk)
		qs := dailyTb.Read(lk)
		mqs := map[string]float64{}
		for _, q := range qs {
			nk, ok := nicksTb.GetNick(lk, q.Nick)
			if ok {
				mqs[nk.Name()] = q.Value
			}
		}
		mans := managersTb.Read(lk)
		for i, m := range mans {
			anns := diariesDb.ReadAnnotations(lk, i)
			ledger, portfolio, _ := acc.Settlement(anns)
			base := ledger.Cash() + ledger.Capital()
			account := base + ledger.Stocks()
			total := base
			risk := base
			for _, e := range portfolio {
				lastQ, ok := mqs[e.Nick()]
				if ok {
					total += float64(e.Stocks()) * lastQ

					me := m.GetModel(e.Nick())
					cs, ok := closes.NickValuesAdd(e.Nick(), lastQ)
					if ok {
						refs := refsDb.MkRefs(
							lk, e.Nick(), dates, cs, me.Model(), me.Params(),
						)
						ref := refs[len(refs)-1]
						if ref > lastQ { // Ref exceed
							ref = lastQ
						}
						risk += float64(e.Stocks()) * ref
					} else {
						risk += float64(e.Stocks()) * e.Price()
					}
				} else {
					log.Error(lk, "Daily quote of "+e.Nick()+" is missing")
					val := float64(e.Stocks()) * e.Price()
					total += val
					risk += val
				}
			}
			profitsDb.Add(lk, i, total, account, risk)
		}
	})
}

func updateDaily(isFinal bool) {
	readSelectedDailyServers := func(lk sync.T) (qs []*nick.QvalueT, ok bool) {
		var svs []*server.T
		for _, e := range serversTb.DailyList(lk) {
			cf, ok2 := e.DailyConf()
			if ok2 {
				if cf.Sel() == cts.ServerSelected {
					svs = append(svs, e)
				}
			}
		}

		var qss [][]*nick.QvalueT
		for _, sv := range svs {
			qs2, ok2 := net.ServerReadDaily(lk, sv)
			if ok2 {
				qss = append(qss, qs2)
			}
		}

		if len(qss) == 0 {
			return
		}

		ok = true

		for i, qs2 := range qss {
			for _, nv := range qs2 {
				nk := nv.Nick
				new := true
				for _, nv2 := range qs {
					if nv2.Nick == nk {
						new = false
						break
					}
				}

				if new {
					nvs := []float64{nv.Value}
					for j := i + 1; j < len(qss); j++ {
						for _, nv2 := range qss[j] {
							if nv2.Nick == nk {
								nvs = append(nvs, nv2.Value)
							}
						}
					}
					qs = append(qs, nick.NewQvalue(nk, fn.MostDup(nvs)))
				}
			}
		}

		return
	}

	mkDailyChart := func(
		closes *qtable.T, hour int, nk *nick.T,
		qs []*nick.QvalueT, oldEntry *dailyChart.T) *dailyChart.T {
		hours := oldEntry.Hours()
		quotes := oldEntry.Quotes()
		missing := true
		for _, q := range qs {
			if q.Nick == nk.Id() {
				hours = append(hours, hour)
				quotes = append(quotes, q.Value)
				missing = false
				break
			}
		}
		if missing {
			hours = append(hours, hour)
			quotes = append(quotes, quotes[len(quotes)-1])
		}
		return dailyChart.New(
			nk.Name(), oldEntry.Close(), hours, quotes, oldEntry.ManagersData(),
		)
	}

	// Main function -----------------------------------------

	sync.Run(func(lk sync.T) {
		sv := sboxTb.GetServer(lk)
		var qs []*nick.QvalueT
		ok := false
		if isFinal {
			qs, ok = readSelectedDailyServers(lk)
		} else {
			qs, ok = net.ServerReadDaily(lk, sv)
		}

		if !ok {
			log.Error(lk, "Fail reading daily server "+sv.ShortName())
			return
		}

		oldDqs := dailyTb.Read(lk)
		if len(oldDqs) != len(qs) {
			return
		}
		eq := true
		for i := 0; i < len(qs); i++ {
			if !fn.Eq(oldDqs[i].Value, qs[i].Value, 0.0000001) {
				eq = false
				break
			}
		}
		if eq {
			return
		}

		dailyTb.Write(lk, qs)

		hour := date.Now().Hour()
		nicks := nicksTb.SelectedNicks(lk)
		closes := quotesDb.Closes(lk)
		oldEntries := dailyChartsTb.Read(lk)
		var entries []*dailyChart.T
		for _, nk := range nicks {
			var new bool
			for _, e := range oldEntries {
				if e.Nick() == nk.Name() {
					entries = append(entries, mkDailyChart(closes, hour, nk, qs, e))
					new = false
					break
				}
			}
			if new {
				hqs := quotesDb.Read(lk, nk.Name())
				if len(qs) == 0 {
					log.Error(lk, "Quotes of "+nk.Name()+" not found")
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
					log.Error(lk, "Every quote of "+nk.Name()+" is not valid")
					continue
				}

				nkCl := &nkClT{nk, cl}
				managers := managersTb.Read(lk)
				var pfMgs []*pfMgT
				for i, e := range managers {
					anns := diariesDb.ReadAnnotations(lk, i)
					_, portfolio, _ := acc.Settlement(anns)
					pfMgs = append(pfMgs, &pfMgT{portfolio, e})
				}
				entries = append(
					entries, mkDailyChartInit(lk, closes, hour, nkCl, qs, pfMgs),
				)
			}
		}
		dailyChartsTb.Write(lk, entries)
	})
}

func updateHistoric() {
	var ls []*nick.T
	sync.Run(func(lk sync.T) {
		nkModel, ok := nicksTb.GetModel(lk)
		if ok {
			ls = append(ls, nkModel)
			modelId := nkModel.Id()
			for _, e := range nicksTb.Nicks(lk) {
				if e.Id() != modelId {
					ls = append(ls, e)
				}
			}
		}
	})

	if len(ls) == 0 {
		sync.Run(func(lk sync.T) {
			log.Error(lk, "Historic updating failed: Nick model not found")
		})
		return
	}

	if stopper.Stop {
		return
	}

	for _, e := range ls {
		sync.Run(func(lk sync.T) {
			net.UpdateHistoric(lk, e.Id())
		})
	}
}

func updatePerformance() {
	sync.Run(func(lk sync.T) {

		// -----
		getOpen := func(
			nick string, date string) (open float64, ok bool,
		) {
			for _, q := range quotesDb.Read(lk, nick) {
				if q.Date() == date {
					ok = true
					open = q.Open()
					return
				}
			}
			return
		}
		// -----

		years := diariesDb.Years(lk)
		endYears := 1
		if len(years) > 1 {
			endYears = 2
		}
		perfData := performanceTb.Read(lk)
		lastDate := "000000"
		if len(perfData) == 0 {
			for i := 0; i < endYears; i++ {
				y := years[i]
				allJs := diariesDb.ReadAllJs(lk, y)
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
			allJs := diariesDb.ReadAllJs(lk, y)
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
		performanceTb.Write(lk, perfData)
	})
}

// Returns the initial state of activity depending on where was the last one.
//    lastAct: Activity when server was stopped by last time.
func InitialActivity(lastAct *activity.T) *activity.T {
	return InitialActivity2(lastAct, date.Now())
}

// Equals than 'initialActivity' for debug.
//    lastAct: Activity when server was stopped by last time.
//    today  : current day.
func InitialActivity2(lastAct *activity.T, today date.T) *activity.T {
	day := lastAct.Date()
	ac := lastAct.Activity()
	switch ac {
	case cts.ActSleeping2:
		if day.Eq(today) {
			if calendarTb.IsOpen(today) {
				return activity.New2(today, cts.ActActivating)
			} else if today.Hour() > 12 && calendarTb.IsMarketDay(today) {
				return activity.New2(today, cts.ActDeactivating)
			} else {
				return activity.New2(today, cts.ActSleeping2)
			}
		} else if calendarTb.PreviousMarketDay(today).Df(day) >= 0 {
			return activity.New2(today, cts.ActHistoric)
		} else {
			return activity.New2(today, cts.ActSleeping2)
		}
	case cts.ActSleeping1:
		return activity.New2(today, cts.ActSleeping1)
	case cts.ActActive:
		if day.Eq(today) {
			return activity.New2(today, cts.ActActive)
		} else {
			return activity.New2(today, cts.ActSleeping1)
		}
	case cts.ActHistoric:
		return activity.New2(today, cts.ActHistoric)
	case cts.ActActivating:
		if day.Eq(today) {
			return activity.New2(today, cts.ActActivating)
		} else {
			return activity.New2(today, cts.ActSleeping1)
		}
	case cts.ActDeactivating:
		if day.Eq(today) {
			return activity.New2(today, cts.ActDeactivating)
		} else {
			return activity.New2(today, cts.ActSleeping1)
		}
	default:
		panic("Activity '" + ac + "' is unknown")
	}
}

func ForceDeactivating() {
	updateDaily(true)
	updateProfitsHistoric()
}

func Start(ch chan int, act *activity.T) {
	// To initialize a new flea model.
	// fleas.Evolution()

	changeActivity := func(newAct string) {
		act = activity.New(newAct)
		sync.Run(func(lk sync.T) {
			conf.SetActivity(lk, act)
			log.Info(lk, newAct)
		})
	}

	for {
		if stopper.Stop {
			break
		}

		switch act.Activity() {
		case cts.ActActivating:
			activating()
			updateProfitsHistoric()
			changeActivity(cts.ActActive)
		case cts.ActActive:
			count := 0
			for {
				if stopper.Stop || !calendarTb.IsOpen(date.Now()) {
					break
				}
				if count >= cts.SchedulerTimes {
					updateDaily(false)
					updateProfitsHistoric()
					count = 0
				}
				count++
				sys.Sleep(cts.SchedulerSleep)
			}
			if !stopper.Stop {
				changeActivity(cts.ActDeactivating)
			}
		case cts.ActDeactivating:
			updateDaily(true)
			updateProfitsHistoric()
			changeActivity(cts.ActSleeping1)
		case cts.ActSleeping1:
			var h int
			for {
				h = date.Now().Hour()
				if stopper.Stop ||
					(h >= cts.ActHistoricStart &&
						(h <= cts.ActHistoricEnd || calendarTb.ForClose(date.Now()))) {
					break
				}
				sys.Sleep(cts.SchedulerSleep)
			}
			if !stopper.Stop {
				changeActivity(cts.ActHistoric)
			}
		case cts.ActHistoric:
			updateHistoric()
			updatePerformance()
			if !stopper.Stop {
				go fleas.Evolution()
				sync.Run(func(lk sync.T) {
					for i := 0; i < cts.Managers; i++ {
						managersTb.Regularize(lk, i)
					}
				})
				changeActivity(cts.ActSleeping2)
			}
		default: // cts.ActSleeping2
			for {
				if stopper.Stop || calendarTb.IsOpen(date.Now()) {
					break
				}
				sys.Sleep(cts.SchedulerSleep)
			}
			if !stopper.Stop {
				changeActivity(cts.ActActivating)
			}
		}
	}

	ch <- 0
}
