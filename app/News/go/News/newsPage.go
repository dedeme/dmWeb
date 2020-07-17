// Copyright 06-Jul-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// News page.
package main

import (
	"fmt"
	"github.com/dedeme/News/control/autoEval"
	"github.com/dedeme/News/control/userEval"
	"github.com/dedeme/News/data/newsEntry"
	"github.com/dedeme/News/data/webSource"
	"github.com/dedeme/News/db/evalWeights"
	"github.com/dedeme/News/db/newss"
	"github.com/dedeme/News/net"
	"github.com/dedeme/golib/cgi"
	"github.com/dedeme/golib/json"
	"sort"
)

func newsProcess(ck string, mrq map[string]json.T) string {
	rq := cgi.RqString(mrq, "rq")
	switch rq {
	case "read":
		newss.WebAdd(newss.LocalRead())
		news := newss.WebRead()
		autoEval.Run(news)
		ew := evalWeights.Read()
		for _, e := range news {
			e.TtEval = e.Seval*ew.Source + e.Aeval*ew.Author + e.Weval*ew.Words
		}
		sort.Slice(
			news,
			func(i, j int) bool {
				cp := news[i].Date.Compare(news[j].Date)
				if cp == 0 {
					return news[i].TtEval > news[j].TtEval
				}
				return cp > 0
			},
		)
		if len(news) > 1000 {
			news = news[:100]
		}
		newss.WebWrite(news)
		return cgi.Rp(ck, map[string]json.T{
			"news": newss.WebReadJs(),
		})
	case "eval":
		entry := newsEntry.FromJs(mrq["entry"])
		ev := cgi.RqInt(mrq, "ev")
		userEval.Run(entry, ev)
		return cgi.RpEmpty(ck)
	case "newsNews":
		var errs []json.T
		newss.WebReset()

		for _, s := range webSource.List() {
			for _, p := range s.Pages() {
				news, err := net.Read(p)
				if err != nil {
					errs = append(
						errs,
						json.Ws(fmt.Sprintf(
							"Fail reading %v(%v):\n%v",
							s.Id(), p.Url, err.Error(),
						)),
					)
				} else {
					newss.WebAdd(news)
				}
			}
		}

		ns := newss.WebRead()
		autoEval.Run(ns)
		newss.WebWrite(ns)

		return cgi.Rp(ck, map[string]json.T{
			"errs": json.Wa(errs),
		})
	default:
		panic(fmt.Sprintf("Value of rq ('%v') is not valid", rq))
	}
}
