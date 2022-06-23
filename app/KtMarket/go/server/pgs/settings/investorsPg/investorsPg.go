// Copyright 05-Jun-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Settings-InvestorsPg page
package investorsPg

import (
	"github.com/dedeme/KtMarket/cts"
	"github.com/dedeme/KtMarket/data/investor"
	"github.com/dedeme/KtMarket/data/model"
	"github.com/dedeme/KtMarket/data/strategy"
	"github.com/dedeme/KtMarket/db"
	"github.com/dedeme/ktlib/arr"
	"github.com/dedeme/ktlib/cgi"
	"github.com/dedeme/ktlib/js"
	"github.com/dedeme/ktlib/log"
	"github.com/dedeme/ktlib/str"
	"github.com/dedeme/ktlib/thread"
)

func Process(ck string, mrq cgi.T) string {
	rq := js.Rs(mrq["rq"])
	switch rq {
	case "idata":
		investorIx := js.Ri(mrq["investorIx"])

		var ok bool
		var inv *investor.T

		thread.Sync(func() {
			if investorIx >= cts.Investors {
				log.Error(str.Fmt(
					"Investor %d out of range [0, %d)", investorIx, cts.Investors,
				))
				return
			}
			inv = db.InvestorsTb().Read().Investors[investorIx]

			ok = true
		})

		return cgi.Rp(ck, cgi.T{
			"ok":        js.Wb(ok),
			"models":    js.Wa(arr.Map(model.List(), model.ToJs)),
			"investor":  investor.ToJs(inv),
			"investors": js.Wi(cts.Investors),
		})
	case "updateAll":
		var ok bool
		thread.Sync(func() {
			err := db.UpdateInvestors()
			if err != "" {
				log.Error(err)
				return
			}
			ok = true
		})
		return cgi.Rp(ck, cgi.T{
			"ok": js.Wb(ok),
		})
	case "update":
		invIx := js.Ri(mrq["investorIx"])
		nickName := js.Rs(mrq["nickName"])
		modelId := js.Rs(mrq["modelId"])
		params := arr.Map(js.Ra(mrq["params"]), js.Rd)

		var ok bool

		thread.Sync(func() {
			md, ok2 := arr.Find(model.List(), func(m *model.T) bool {
				return m.Id == modelId
			})
			if !ok2 {
				log.Error("Model " + modelId + " not found")
				return
			}

			invsDb := db.InvestorsTb()
			invs := invsDb.Read().Investors
			if invIx >= len(invs) {
				log.Error(str.Fmt(
					"Investor number %d of %d", invIx, len(invs),
				))
				return
			}

			inv := invs[invIx]

			if nickName != "" {
				_, ok2 := inv.Nicks[nickName]
				if !ok2 {
					log.Error("Nick " + nickName + " not found")
					return
				}
				inv.Nicks[nickName] = strategy.New(md, params)

				invsDb.Write(investor.NewTb(invs))

				ok = true
				return
			}

			inv.Base = strategy.New(md, params)
			invsDb.Write(investor.NewTb(invs))

			err := db.UpdateInvestors()
			if err != "" {
				log.Error(err)
				return
			}
			ok = true
		})

		return cgi.Rp(ck, cgi.T{
			"ok": js.Wb(ok),
		})

	default:
		panic("Value of rq (" + rq + ") is not valid")
	}
}
