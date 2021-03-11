// Copyright 27-Aug-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Net management.
package net

import (
	"fmt"
	"github.com/dedeme/MultiMarket/data/cts"
	"github.com/dedeme/MultiMarket/data/nick"
	"github.com/dedeme/MultiMarket/data/quote"
	"github.com/dedeme/MultiMarket/data/server"
	"github.com/dedeme/MultiMarket/db/log"
	"github.com/dedeme/MultiMarket/db/nicksTb"
	"github.com/dedeme/MultiMarket/db/quotesDb"
	"github.com/dedeme/MultiMarket/db/serversTb"
	"github.com/dedeme/MultiMarket/global/sync"
	"github.com/dedeme/golib/file"
	"github.com/dedeme/golib/sys"
	"strconv"
	"strings"
)

type dailyEntryT struct {
	code  string
	close float64
}

type historicEntryT struct {
	date  string
	open  float64
	close float64
	max   float64
	min   float64
	vol   int
}

func allDigits(s string) bool {
	for i := 0; i < len(s); i++ {
		ch := s[1]
		if ch < 48 || ch > 57 {
			return false
		}
	}
	return true
}

// Download a URL
func download(lk sync.T, cmd string, url string) string {
	if cmd == "Wget" {
		waiting := true
		tm := 0
		r := ""
		go func() {
			br, _ := sys.Cmd("wget", "-q", "--no-cache", "-O", "-", url)
			waiting = false
			r = string(br)
		}()
		for waiting && tm < cts.WebWait {
			sys.Sleep(100)
			tm += 100
		}
		return r
	} else {
		tmp := file.TempFile("", "libdmyeti")
		tmpName := tmp.Name()
		cm := fmt.Sprintf("node -e \""+
			"const puppeteer = require('puppeteer');"+
			"(async () => {"+
			"  try {"+
			"    const browser = await puppeteer.launch();"+
			"    const page = await browser.newPage();"+
			"    page.setDefaultNavigationTimeout(%v);"+
			"    await page.goto('%v',{waitUntil:'domcontentloaded'});"+
			"    const ct = await page.content();"+
			"    console.log(ct);"+
			"    await browser.close();"+
			"  } catch (e) {"+
			"    console.error(e.toString());"+
			"    process.exit(1);"+
			"  }"+
			"})();"+
			"\" 2>/dev/null",
			cts.WebWait, url,
		)
		file.Write(tmp, cm)
		tmp.Close()
		br, _ := sys.Cmd("bash", tmpName)
		file.Remove(tmpName)
		return string(br)
	}
}

// Regularize number.
func toNumber(lk sync.T, isIso bool, n string) (nn float64, ok bool) {
	if isIso {
		n = strings.ReplaceAll(strings.ReplaceAll(n, ".", ""), ",", ".")
	} else {
		n = strings.ReplaceAll(n, ",", "")
	}

	nn, err := strconv.ParseFloat(n, 64)
	if err != nil {
		if n == "" || strings.HasPrefix(n, "-") {
			nn = 0
			ok = true
			return
		}
		log.Error(lk, fmt.Sprintf("Wrong number (%v)", n))
		return
	}
	if nn < 0 {
		log.Error(lk, fmt.Sprintf("Negative quote (%v)", nn))
		return
	}
	ok = true
	return
}

// Regularize date.
func mkDate(lk sync.T, d string, sep string, isIso bool) (dd string, ok bool) {
	readYahoo := func() (yd string, yok bool) {
		months := []string{
			"ene.", "feb.", "mar.", "abr.", "may.", "jun.",
			"jul.", "ago.", "sept.", "oct.", "nov.", "dic.",
		}
		parts := strings.Split(d, " ")
		if len(parts) != 3 {
			log.Error(lk, fmt.Sprintf("Wrong Yahoo date (%v)", d))
			return
		}
		day := parts[0]
		lday := len(day)
		month := 0
		pmonth := parts[1]
		for i, e := range months {
			if e == pmonth {
				month = i + 1
				break
			}
		}
		year := parts[2]
		lyear := len(year)
		if month > 0 &&
			allDigits(day) && lday > 0 && lday < 3 &&
			allDigits(year) && (lyear == 2 || lyear == 4) {
			if lday == 1 {
				day = "0" + day
			}
			smonth := strconv.Itoa(month)
			if len(smonth) == 1 {
				smonth = "0" + smonth
			}
			if lyear == 2 {
				year = "20" + year
			}

			yd = year + smonth + day
			yok = true
			return
		}
		log.Error(lk, fmt.Sprintf("Wrong Yahoo date (%v)", d))
		return
	}

	if sep == "*" {
		dd, ok = readYahoo()
		return
	}

	parts := strings.Split(d, sep)
	if len(parts) != 3 {
		log.Error(lk, fmt.Sprintf("Wrong date (%v)", d))
		return
	}

	month := parts[0]
	day := parts[1]
	if isIso {
		month = parts[1]
		day = parts[0]
	}
	year := parts[2]
	lday := len(day)
	lmonth := len(month)
	lyear := len(year)

	if allDigits(day) && lday > 0 && lday < 3 &&
		allDigits(month) && lmonth > 0 && lmonth < 3 &&
		allDigits(year) && (lyear == 2 || lyear == 4) {
		if lday == 1 {
			day = "0" + day
		}
		if lmonth == 1 {
			month = "0" + month
		}
		if lyear == 2 {
			year = "20" + year
		}

		dd = year + month + day
		ok = true
		return
	}
	log.Error(lk, fmt.Sprintf("Wrong date (%v)", d))
	return
}

// Returns html rest after group of start marks or ok=false if the group is
// not found.
func goStart(html string, marks []string) (rest string, ok bool) {
	if len(marks) == 0 {
		panic("There are no start marks")
	}
	for _, e := range marks {
		ix := strings.Index(html, e)
		if ix == -1 {
			return
		}
		html = html[ix+len(e):]
	}
	rest = html
	ok = true
	return
}

// Returns html text and rest after group of end marks or ok=false if the group
// is not found.
func goEnd(html string, marks []string) (text, rest string, ok bool) {
	if len(marks) == 0 {
		panic("There are no end marks")
	}
	for {
		mk0 := marks[0]
		ix := strings.Index(html, mk0)
		if ix == -1 {
			return
		}
		text = text + html[:ix]
		rest, ok = goStart(html[ix:], marks)
		if ok {
			return
		}
		text += marks[0]
		html = html[ix+len(mk0):]
	}
}

// Read a configuration page.
// If 'conf' is a daily configuration, 'code' is an empty string.
// If reading fails, 'ok == false'.
func read(
	lk sync.T, conf *server.ConfT, code string,
) (table [][]string, ok bool) {

	getHtml := func() (html string, ok bool) {
		url := conf.Url()
		if code != "" {
			url = strings.ReplaceAll(url, "${code}", code)
		}
		html = download(lk, conf.Cmd(), url)
		if html == "" {
			log.Error(lk, fmt.Sprintf("Inet error reading '%v'", url))
			return
		}
		html, ok = goStart(html, strings.Split(conf.TableStart(), "|"))
		if !ok {
			log.Error(lk, fmt.Sprintf("Start table not found reading '%v'", url))
			return
		}
		html, _, ok = goEnd(html, strings.Split(conf.TableEnd(), "|"))
		if !ok {
			log.Error(lk, fmt.Sprintf("End table not found reading '%v'", url))
		}
		return
	}

	html, ok := getHtml()
	if !ok {
		return
	}

	rowStart := strings.Split(conf.RowStart(), "|")
	rowEnd := strings.Split(conf.RowEnd(), "|")
	var cellsStart [][]string
	for _, e := range conf.CellsStart() {
		cellsStart = append(cellsStart, strings.Split(e, "|"))
	}
	var cellsEnd [][]string
	for _, e := range conf.CellsEnd() {
		cellsEnd = append(cellsEnd, strings.Split(e, "|"))
	}
	if len(cellsStart) != len(cellsEnd) {
		panic(fmt.Sprintf(
			"Length of start cells != length of end cells (Start: %v, End: %v)",
			len(cellsStart), len(cellsEnd),
		))
	}
	if len(cellsStart) == 0 {
		panic("Length of start cells == 0")
	}

	for {
		var row []string
		html, ok = goStart(html, rowStart)
		if !ok {
			break
		}

		var htmlRow string
		htmlRow, html, ok = goEnd(html, rowEnd)
		if !ok {
			break
		}

		for i := 0; i < len(cellsStart); i++ {
			htmlRow, ok = goStart(htmlRow, cellsStart[i])
			if !ok {
				break
			}
			var text string
			text, htmlRow, ok = goEnd(htmlRow, cellsEnd[i])
			if !ok {
				break
			}
			row = append(row, strings.TrimSpace(text))
		}
		if ok {
			table = append(table, row)
		}
	}

	ok = true
	return
}

func readDaily(
	lk sync.T, conf *server.ConfT,
) (entries []*dailyEntryT, ok bool) {
	table, ok := read(lk, conf, "")
	if !ok {
		return
	}
	ixC := strings.Index(conf.FieldsType(), "C")
	if ixC == -1 {
		panic("Code field is missing")
	}
	ixQ := strings.Index(conf.FieldsType(), "Q")
	if ixQ == -1 {
		panic("Quote field is missing")
	}

	for _, row := range table {
		close, ok := toNumber(lk, conf.IsIsoNumber(), row[ixQ])
		if ok {
			entries = append(entries, &dailyEntryT{row[ixC], close})
		} else {
      log.Info(lk, "Fail reading quote of '" + row[ixC] + "'. Quote sets to 0.")
    }
	}

	if len(table) > 0 {
		ok = true
	}
	return
}

func readHistoric(
	lk sync.T, conf *server.ConfT, code string,
) (entries []*historicEntryT, ok bool) {
	table, tOk := read(lk, conf, code)
	if !tOk {
		return
	}

	ixD := strings.Index(conf.FieldsType(), "D")
	if ixD == -1 {
		panic("Date field is missing")
	}
	ixO := strings.Index(conf.FieldsType(), "O")
	if ixO == -1 {
		panic("Open field is missing")
	}
	ixC := strings.Index(conf.FieldsType(), "C")
	if ixC == -1 {
		panic("Close field is missing")
	}
	ixX := strings.Index(conf.FieldsType(), "X")
	if ixX == -1 {
		panic("Maximum field is missing")
	}
	ixN := strings.Index(conf.FieldsType(), "N")
	if ixN == -1 {
		panic("Minimum field is missing")
	}
	ixV := strings.Index(conf.FieldsType(), "V")
	if ixV == -1 {
		panic("Volume field is missing")
	}

	for _, row := range table {
		date, ok := mkDate(lk, row[ixD], conf.DateSeparator(), conf.IsIsoDate())
		if !ok {
			continue
		}
		open, ok := toNumber(lk, conf.IsIsoNumber(), row[ixO])
		if !ok {
			continue
		}
		close, ok := toNumber(lk, conf.IsIsoNumber(), row[ixC])
		if !ok {
			continue
		}
		max, ok := toNumber(lk, conf.IsIsoNumber(), row[ixX])
		if !ok {
			continue
		}
		min, ok := toNumber(lk, conf.IsIsoNumber(), row[ixN])
		if !ok {
			continue
		}
		volf, ok := toNumber(lk, conf.IsIsoNumber(), row[ixV])
		if !ok {
			continue
		}
		vol := int(volf)
		entries = append(entries, &historicEntryT{date, open, close, max, min, vol})
	}

	if len(table) > 0 {
		ok = true
	}
	return
}

// Reads daily quotes.
//
// Companies without server code or whose code is not found generate a
// quote value of '0' and produce a log error.
//   lk: Synchronization lock.
//   sv: Web server.
func ServerReadDaily(lk sync.T, sv *server.T) (qvs []*nick.QvalueT, ok bool) {
	conf, ok := sv.DailyConf()
	if !ok {
		log.Error(lk, fmt.Sprintf(
			"Server %v: Daily configuration not defined", sv.Name(),
		))
		return
	}

	table, ok := readDaily(lk, conf)
	if !ok {
		log.Error(lk, fmt.Sprintf(
			"Fail reading daily data of '%v'", sv.Name(),
		))
		return
	}

	for _, nkCd := range sv.Codes() {
		nickId := nkCd.NickId()
		value := float64(0)
		c, ok := nkCd.Code()
		if ok {
			for _, e := range table {
				if e.code == c {
					value = e.close
					break
				}
			}
		} else {
			nk, ok := nicksTb.GetNick(lk, nickId)
			nkName := strconv.Itoa(nickId)
			if ok {
				nkName = nk.Name()
			}
			log.Error(lk, fmt.Sprintf(
				"Server '%v' has not defined code for '%v'", sv.Name(), nkName,
			))
		}
		qvs = append(qvs, &nick.QvalueT{nickId, value})
	}

	return
}

//  Reads historic quotes.
//    lk: Synchronization lock.
//    sv: Web server.
//    nk: Company nick.
func ServerReadHistoric(
	lk sync.T, sv *server.T, nk *nick.T,
) (qs []*quote.T, ok bool) {
	nickId := nk.Id()

	tryRead := func(
		times int, conf *server.ConfT, code string,
	) (qs []*quote.T, ok bool) {
		for i := 0; i < times; i++ {
			entries, eOk := readHistoric(lk, conf, code)
			if eOk {
				if len(entries) < cts.HistoricMinimumEntries {
					if i+1 >= times {
						log.Error(lk, fmt.Sprintf(
							"Historic entries of nick '%v' in server '%v' are less than "+
								"%v (%v)",
							nk.Name(), sv.Name(), cts.HistoricMinimumEntries, len(entries),
						))
						return
					}
					continue
				}
				for _, e := range entries {
					qs = append(qs, quote.New(
						e.date, e.open, e.close, e.max, e.min, e.vol, false,
					))
				}
				ok = true
				break
			}
		}
		if !ok {
			log.Error(lk, fmt.Sprintf("Historic server '%v' not read", sv.Name()))
		}
		return
	}

	conf, hOk := sv.HistoricConf()
	if !hOk {
		log.Error(lk, fmt.Sprintf(
			"Server %v: Historic configuration not defined", sv.Name(),
		))
		return
	}

	for _, nkCd := range sv.Codes() {
		if nkCd.NickId() == nickId {
			code, cOk := nkCd.Code()
			if !cOk {
				log.Error(lk, fmt.Sprintf(
					"Server '%v' has not defined code for '%v'",
					sv.Name(), nk.Name(),
				))
				return
			}
			qs, ok = tryRead(2, conf, code)
			return
		}
	}

	log.Error(lk, fmt.Sprintf(
		"Code of '%v' is missing in '%v'",
		nk.Name(), sv.Name(),
	))
	return
}

// Test daily configuration and returns 'true' if the result was ok.v
//    lk: Synchronization lock.
//    id: Server identifier.
func TestDailyConf(lk sync.T, id int) bool {
	for _, sv := range serversTb.Read(lk) {
		if sv.Id() == id {
			nickCloses, ok := ServerReadDaily(lk, sv)
			if ok && len(nickCloses) == len(sv.Codes()) {
				return true
			}
			return false
		}
	}
	return false
}

// Test historic configuration and returns 'true' if the result was ok.v
//    lk  : Synchronization lock.
//    id  : Server identifier.
//    nick: Company nick.
func TestHistoricConf(lk sync.T, id int, nk *nick.T) bool {
	for _, sv := range serversTb.Read(lk) {
		if sv.Id() == id {
			_, ok := ServerReadHistoric(lk, sv, nk)
			if ok {
				return true
			}
			return false
		}
	}
	return false
}

// Updates historic quotes of "nickId".
//    lk    : Synchronization lock.
//    nickId: company nick identifier.
func UpdateHistoric(lk sync.T, nickId int) (withErrors, withWarnings bool) {
	nk, ok := nicksTb.GetNick(lk, nickId)
	if !ok {
		log.Error(lk, fmt.Sprintf("Nick id '%v' not found", nickId))
		withErrors = true
		return
	}

	oldQs := quotesDb.Read(lk, nk.Name())
	if len(oldQs) == 0 {
		log.Error(lk, fmt.Sprintf("Fail reading quotes from %v.tb", nk.Name()))
		withErrors = true
		return
	}

	nkModel, ok := nicksTb.GetModel(lk)
	if !ok {
		log.Error(lk, "Nick model not defined")
		withErrors = true
		return
	}

	var modelQs []*quote.T
	if nkModel.Id() != nk.Id() {
		modelQs = quotesDb.Read(lk, nkModel.Name())
		if len(modelQs) == 0 {
			log.Error(lk, fmt.Sprintf(
				"Fail reading model quotes from %v.tb", nkModel.Name(),
			))
			withErrors = true
			return
		}
	}

	log.Info(lk, fmt.Sprintf("Updating %v", nk.Name()))
	servers, sel := serversTb.HistoricList(lk)
	var svQs [][]*quote.T
	svSel := 0
	for i, sv := range servers {
		qs, ok := ServerReadHistoric(lk, sv, nk)
		if ok {
			svQs = append(svQs, qs)
			if i == sel {
				svSel = len(svQs) - 1
			}
		}
	}
	if len(svQs) == 0 {
		log.Error(lk, fmt.Sprintf(
			"No server can read quotes of %v", nk.Name(),
		))
		withErrors = true
		return
	}

	newQs := quote.Unify(svQs, svSel)
	qs, serrors := quote.Merge(modelQs, newQs, oldQs)

	ok = quotesDb.SetQuotes(lk, nk.Id(), qs)
	if !ok {
		log.Error(lk, fmt.Sprintf(
			"Fail writing quotes of %v", nk.Name(),
		))
		withErrors = true
		return
	}

	withWarnings = len(serrors) > 0
	if withWarnings {
		log.Error(lk, strings.Join(serrors, "\n"))
	}

	return
}
