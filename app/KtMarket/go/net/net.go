// Copyright 21-Oct-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Net management.
package net

import (
	"github.com/dedeme/KtMarket/cts"
	"github.com/dedeme/KtMarket/data/nick"
	"github.com/dedeme/KtMarket/data/quote"
	"github.com/dedeme/KtMarket/data/server"
	"github.com/dedeme/KtMarket/db"
	"github.com/dedeme/ktlib/file"
	"github.com/dedeme/ktlib/math"
	"github.com/dedeme/ktlib/regex"
	"github.com/dedeme/ktlib/str"
	"github.com/dedeme/ktlib/sys"
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
func download(cmd string, url string) string {
	if cmd == cts.Wget {
		waiting := true
		tm := 0
		r := ""
		go func() {
			br, _ := sys.Cmd(
				"wget",
				cts.WgetUA1, cts.WgetUA2, cts.WgetUA3,
				"-q", "--no-cache", "-O", "-", url,
			)
			waiting = false
			r = string(br)
		}()
		for waiting && tm < cts.WebWait {
			sys.Sleep(100)
			tm += 100
		}
		return r
	}
	if cmd == cts.Puppeteer {
		tmpName := file.Tmp("/tmp", "KtMarket")
		cm := str.Fmt("node -e \""+
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
		file.Write(tmpName, cm)
		br, _ := sys.Cmd("bash", tmpName)
		file.Del(tmpName)
		return string(br)
	}

	panic("'" + cmd + "' is no a valid program to read web pages")
}

// Regularize number.
func toNumber(isIso bool, n string) (nn float64, err string) {
	defer func() {
		if err := recover(); err != nil {
			if n == "" || str.Starts(n, "-") {
				nn = 0.0
			} else {
				err = str.Fmt("Wrong number reading server (%v)", n)
			}
		}
	}()

	if isIso {
		n = str.Replace(str.Replace(n, ".", ""), ",", ".")
	} else {
		n = str.Replace(n, ",", "")
	}

	nn = math.FromStr(n)
	if nn < 0 {
		err = str.Fmt("Negative quote reading server (%v)", nn)
		return
	}
	return
}

// Regularize date.
func mkDate(d string, sep string, isIso bool) (dd string, err string) {
	readYahoo := func() (yd string, yerr string) {
		months := []string{
			"ene", "feb", "mar", "abr", "may", "jun",
			"jul", "ago", "sept", "oct", "nov", "dic",
		}
		parts := str.Split(d, " ")
		if len(parts) != 3 {
			yerr = str.Fmt("Wrong Yahoo date (%v)", d)
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
			smonth := str.Fmt("%d", month)
			if len(smonth) == 1 {
				smonth = "0" + smonth
			}
			if lyear == 2 {
				year = "20" + year
			}

			yd = year + smonth + day
			return
		}
		yerr = str.Fmt("Wrong Yahoo date (%v)", d)
		return
	}

	if sep == "*" {
		dd, err = readYahoo()
		return
	}

	parts := str.Split(d, sep)
	if len(parts) != 3 {
		err = str.Fmt("Wrong date reading server (%v)", d)
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
		return
	}
	err = str.Fmt("Wrong date reading server (%v)", d)
	return
}

// Returns html rest after group of start marks.
func goStart(html string, marks []string) (rest string, err string) {
	if len(marks) == 0 {
		err = "Start marks are missing."
		return
	}
	for _, e := range marks {
		ix := str.Index(html, e)
		if ix == -1 {
			err = "Mark '" + e + "'not found reading web page."
			return
		}
		html = html[ix+len(e):]
	}
	rest = html
	return
}

// Returns html text and rest after group of end marks or ok=false if the group
// is not found.
func goEnd(html string, marks []string) (text, rest string, err string) {
	if len(marks) == 0 {
		err = "There are no end marks in web page."
	}

	for {
		mk0 := marks[0]

		ix := str.Index(html, mk0)
		if ix == -1 {
			err = "Mark '" + mk0 + "'not found reading web page.\n" + html
			return
		}
		text = text + html[:ix]
		rest, err = goStart(html[ix:], marks)
		if err == "" {
			return
		}
		text += marks[0]
		html = html[ix+len(mk0):]
	}
}

// Read a configuration page.
// If 'conf' is a daily configuration, 'code' is an empty string.
// If some error happends which allows to go on reading, a warning is added.
func read(
	conf *server.ConfT, code string,
) (table [][]string, warnings []string, err string) {
	getHtml := func() (html string, herr string) {
		url := conf.Url
		if code != "" {
			url = str.Replace(url, "${code}", code)
		}
		html = download(conf.Cmd, url)

		if html == "" {
			herr = str.Fmt("Inet error reading '%v'", url)
			return
		}

		if str.Trim(conf.Regex) != "" {
			exps := str.Split(conf.Regex, "\n")
			for _, l := range exps {
				ps := str.Split(str.Trim(l), "|x|")
				if len(ps) == 2 {
					exp := str.Trim(ps[0])
					if exp != "" {
						html = regex.Replace(html, exp, str.Trim(ps[1]))
					}
				}
			}
		}

		html, herr = goStart(html, str.Split(conf.TableStart, "|"))
		if herr != "" {
			return
		}

		html, _, herr = goEnd(html, str.Split(conf.TableEnd, "|"))
		return
	}

	html, err := getHtml()
	if err != "" {
		return
	}

	rowStart := str.Split(conf.RowStart, "|")
	rowEnd := str.Split(conf.RowEnd, "|")
	var cellsStart [][]string
	for _, e := range conf.CellsStart {
		cellsStart = append(cellsStart, str.Split(e, "|"))
	}
	var cellsEnd [][]string
	for _, e := range conf.CellsEnd {
		cellsEnd = append(cellsEnd, str.Split(e, "|"))
	}
	if len(cellsStart) != len(cellsEnd) {
		err = str.Fmt(
			"Length of start cells != length of end cells (Start: %v, End: %v)",
			len(cellsStart), len(cellsEnd),
		)
		return
	}
	if len(cellsStart) == 0 {
		err = "Length of start cells == 0"
		return
	}

	for {
		var werr string
		var row []string
		html, werr = goStart(html, rowStart)
		if werr != "" {
			break
		}

		var htmlRow string
		htmlRow, html, werr = goEnd(html, rowEnd)
		if werr != "" {
			warnings = append(warnings, werr)
			break
		}

		for i := 0; i < len(cellsStart); i++ {
			htmlRow, werr = goStart(htmlRow, cellsStart[i])
			if werr != "" {
				break
			}
			var text string
			text, htmlRow, werr = goEnd(htmlRow, cellsEnd[i])
			if werr != "" {
				warnings = append(warnings, werr)
				break
			}
			row = append(row, str.Trim(text))
		}
		if werr == "" {
			table = append(table, row)
		}
	}

	return
}

// If some error happends which allows to go on reading, a warning is added.
func readDaily(
	conf *server.ConfT,
) (entries []*dailyEntryT, warnings []string, err string) {
	table, warnings, err := read(conf, "")
	if err != "" {
		return
	}
	ixC := str.Index(conf.FieldsType, "C")
	if ixC == -1 {
		err = "Code field is missing in server configuration"
		return
	}
	ixQ := str.Index(conf.FieldsType, "Q")
	if ixQ == -1 {
		err = "Quote field is missing in server configuration"
		return
	}

	for _, row := range table {
		close, werr := toNumber(conf.IsIsoNumber, row[ixQ])
		if werr == "" {
			entries = append(entries, &dailyEntryT{row[ixC], close})
		} else {
			warnings = append(
				warnings,
				"Fail reading quote of '"+row[ixC]+"'. Quote sets to 0.",
			)
		}
	}

	if len(table) == 0 {
		err = "Quotes table read from web page is empty"
	}

	return
}

// If some error happends which allows to go on reading, a warning is added.
func readHistoric(
	conf *server.ConfT, code string,
) (entries []*historicEntryT, warnings []string, err string) {
	table, warnings, err := read(conf, code)
	if err != "" {
		return
	}

	ixD := str.Index(conf.FieldsType, "D")
	if ixD == -1 {
		err = "Date field is missing in server configuration"
		return
	}
	ixO := str.Index(conf.FieldsType, "O")
	if ixO == -1 {
		err = "Open field is missing in server configuration"
		return
	}
	ixC := str.Index(conf.FieldsType, "C")
	if ixC == -1 {
		err = "Close field is missing in server configuration"
		return
	}
	ixX := str.Index(conf.FieldsType, "X")
	if ixX == -1 {
		err = "Maximum field is missing in server configuration"
		return
	}
	ixN := str.Index(conf.FieldsType, "N")
	if ixN == -1 {
		err = "Minimum field is missing in server configuration"
		return
	}
	ixV := str.Index(conf.FieldsType, "V")
	if ixV == -1 {
		err = "Volume field is missing in server configuration"
		return
	}

	for _, row := range table {
		date, werr := mkDate(row[ixD], conf.DateSeparator, conf.IsIsoDate)
		if werr != "" {
			warnings = append(warnings, werr)
			continue
		}
		open, werr := toNumber(conf.IsIsoNumber, row[ixO])
		if werr != "" {
			warnings = append(warnings, werr)
			continue
		}
		close, werr := toNumber(conf.IsIsoNumber, row[ixC])
		if werr != "" {
			warnings = append(warnings, werr)
			continue
		}
		max, werr := toNumber(conf.IsIsoNumber, row[ixX])
		if werr != "" {
			warnings = append(warnings, werr)
			continue
		}
		min, werr := toNumber(conf.IsIsoNumber, row[ixN])
		if werr != "" {
			warnings = append(warnings, werr)
			continue
		}
		volf, werr := toNumber(conf.IsIsoNumber, row[ixV])
		if werr != "" {
			warnings = append(warnings, werr)
			continue
		}
		vol := int(volf)
		entries = append(entries, &historicEntryT{date, open, close, max, min, vol})
	}

	if len(table) == 0 {
		err = "Quotes table read from web page is empty"
	}
	return
}

// Read one daily server
func OneServerReadDaily(
	sv *server.T,
) (qvs []*nick.IdValT, warnings []string, err string) {
	conf, ok := sv.DailyConf()
	if !ok {
		err = str.Fmt("Server %v: Daily configuration not defined", sv.Name)
		return
	}

	table, warnings, err := readDaily(conf)
	if err != "" {
		return
	}

	for _, nkCd := range sv.Codes {
		nickId := nkCd.NickId
		value := float64(-1)
		c, ok := nkCd.Code()
		if ok {
			for _, e := range table {
				if e.code == c {
					value = e.close
					break
				}
			}
			if value < 0 {
				warnings = append(warnings, str.Fmt(
					"Server '%v' daily code '%v' read but not defined", sv.Name, c,
				))
			}
		} else {
			nk, ok := db.NicksTb().Read().NickFromId(nickId)
			nkName := str.Fmt("%d", nickId)
			if ok {
				nkName = nk.Name
			}
			warnings = append(warnings, str.Fmt(
				"Server '%v' has not defined code for '%v'", sv.Name, nkName,
			))
		}
		qvs = append(qvs, nick.NewIdVal(nickId, value))
	}

	return
}

// Reads daily quotes.
//
// Companies without server code or whose code is not found generate a
// quote value of -1 and produce a log error.
//   sv: Web server.
// If some error happends which allows to go on reading, a warning is added.
//    -------
//    Returns:
//      qvs     : Quotes-Value read.
//      warnings: List of warnigs. If 'err!=""' its value is undetermined.
//      error   : error found.
func ServerReadDaily(
	sv *server.T,
) (qvs []*nick.IdValT, warnings []string, err string) {
	svsTb := db.ServersTb().Read()
	svs, sel := svsTb.DailyList()
	qvs, warnings, err = OneServerReadDaily(sv)
	if err != "" {
		qvs, warnings, err = OneServerReadDaily(svs[sel])
		if err != "" {
			for _, sv2 := range svs {
				qvs, warnings, err = ServerReadDaily(sv2)
				if err == "" {
					break
				}
			}
			if err != "" {
				err = "net/ServerReadDaily: No server can read daily quotes"
			}
		}
	}

	return
}

//  Reads historic quotes.
//    sv: Web server.
//    nk: Company nick.
// If some error happends which allows to go on reading, a warning is added.
//    -------
//    Returns:
//      qs      : Quotes read.
//      warnings: List of warnigs. If 'err!=""' its value is undetermined.
//      error   : error found.
func ServerReadHistoric(
	sv *server.T, nk *nick.T,
) (qs []*quote.T, warnings []string, err string) {
	nickId := nk.Id

	tryRead := func(
		times int, cf *server.ConfT, code string,
	) (qs []*quote.T, terr string, twarnings []string) {
		for i := 0; i < times; i++ {
			var entries []*historicEntryT
			entries, twarnings, terr = readHistoric(cf, code)
			if terr != "" {
				continue
			}
			if len(entries) < cts.HistoricMinimumEntries {
				if i+1 >= times {
					terr = str.Fmt(
						"Historic entries of nick '%v' in server '%v' are less than "+
							"%v (%v)",
						nk.Name, sv.Name, cts.HistoricMinimumEntries, len(entries),
					)
					break
				}
				continue
			}
			for _, e := range entries {
				qs = append(qs, quote.New(
					e.date, e.open, e.close, e.max, e.min, e.vol, false,
				))
			}
			break
		}

		return
	}

	conf, hOk := sv.HistoricConf()
	if !hOk {
		err = str.Fmt("Server %v: Historic configuration not defined", sv.Name)
		return
	}

	for _, nkCd := range sv.Codes {
		if nkCd.NickId == nickId {
			code, cOk := nkCd.Code()
			if !cOk {
				err = str.Fmt(
					"Server '%v' has not defined code for '%v'",
					sv.Name, nk.Name,
				)
				return
			}
			qs, err, warnings = tryRead(2, conf, code)
			if err == "" {
				for i, e := range warnings {
					warnings[i] = "[" + nk.Name + "] " + e
				}
			}
			return
		}
	}

	err = str.Fmt("Code of '%v' is missing in '%v'", nk.Name, sv.Name)
	return
}

// Test daily configuration and returns 'true' if the result was ok.v
//    id: Server identifier.
//    -------
//    Returns:
//      warnings: List of warnigs. If 'err!=""' its value is undetermined.
//      error   : error found.
func TestDailyConf(id int) (warnings []string, err string) {
	for _, sv := range db.ServersTb().Read().List {
		if sv.Id == id {
			var nickCloses []*nick.IdValT
			nickCloses, warnings, err = OneServerReadDaily(sv)
			if err == "" && len(nickCloses) == len(sv.Codes) {
				// Show test result ----------------------------
				/*
					codes := sv.Codes
					for i := 0; i < len(nickCloses); i++ {
						nick, _ := nick.GetNick(db.NicksTb().GetNick(codes[i].NickId()))
						sys.Println(str.Fmt("%v = %v", nick, nickCloses[i]))
					}
				*/
				// ---------------------------------------------

				return
			}
			if len(nickCloses) != len(sv.Codes) {
				err = str.Fmt(
					"%d companies have not been read",
					len(sv.Codes)-len(nickCloses),
				)
			}
			return
		}
	}
	err = "Server identifier not found"
	return
}

// Test historic configuration.
//    id  : Server identifier.
//    nick: Company nick.
//    -------
//    Returns:
//      warnings: List of warnigs. If 'err!=""' its value is undetermined.
//      error   : error found.
func TestHistoricConf(id int, nk *nick.T) (warnings []string, err string) {
	for _, sv := range db.ServersTb().Read().List {
		if sv.Id == id {
			_, warnings, err = ServerReadHistoric(sv, nk)
			return
		}
	}
	err = "Server identifier not found"
	return
}

// Updates historic quotes of "nickId".
//    nickName: company nick.
//    -------
//    Returns:
//      warnings: List of warnigs. If 'err!=""' its value is undetermined.
//      error   : error found.
func UpdateHistoric(nk *nick.T) (warnings []string, err string) {
	head := "net/UpdateHistoric [" + nk.Name + "]:"
	readQs := func(nkName string) (qs []*quote.T, rerr string) {
		qs, err := db.QsRead(nkName)
		if err != "" {
			err = head + "\n" + err
		}
		return
	}

	var oldQs []*quote.T
	oldQs, err = readQs(nk.Name)
	if err != "" {
		return
	}

	nksTb := db.NicksTb().Read()
	nkModelId := nksTb.Model
	if nkModelId == -1 {
		err = head + " Nick model not defined"
		return
	}
	nkModel, ok := nksTb.NickFromId(nkModelId)
	if !ok {
		err = str.Fmt(
			head+" Nick model id '%v' not found", nkModelId,
		)
		return
	}

	var modelQs []*quote.T
	if nkModelId != nk.Id {
		modelQs, err = readQs(nkModel.Name)
		if err != "" {
			return
		}
	}

	svsTb := db.ServersTb().Read()
	svs, sel := svsTb.HistoricList()
	newQs, warns, err := ServerReadHistoric(svs[sel], nk)
	if err != "" {
		for _, sv := range svs {
			newQs, warns, err = ServerReadHistoric(sv, nk)
			if err == "" {
				break
			}
		}
		if err != "" {
			err = str.Fmt(
				"net/UpdateHistoric: No server can read quotes of %v", nk.Name,
			)
			return
		}
	}
	warnings = append(warnings, warns...)

	qs, serrors := quote.Merge(modelQs, newQs, oldQs)
	for _, w := range serrors {
		warnings = append(warnings, "["+nk.Name+"] "+w)
	}

	ok = db.QsWrite(nk.Name, qs)
	if !ok {
		err = str.Fmt(head+" File %v.tb not found", nk.Name)
	}

	return
}

// Reads ibex and euro stoxx values from Infobolsa.
func ReadIndexes() (ixs []float64, err string) {
	ixs = make([]float64, 2)
	url := "https://www.infobolsa.es/indices/mundiales"

	html := download(cts.Wget, url)

	ix := str.Index(html, "\"IBEX 35\"")
	ix = str.IndexFrom(html, "price ", ix)
	ix = str.IndexFrom(html, ">", ix) + 1
	ix2 := str.IndexFrom(html, "<", ix)
	ixs[0], err = toNumber(true, str.Trim(html[ix:ix2]))
	if err != "" {
		return
	}

	ix = str.Index(html, "\"EURO STOXX50\"")
	ix = str.IndexFrom(html, "price ", ix)
	ix = str.IndexFrom(html, ">", ix) + 1
	ix2 = str.IndexFrom(html, "<", ix)
	ixs[1], err = toNumber(true, str.Trim(html[ix:ix2]))

	return
}
