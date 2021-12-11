// Copyright 21-Oct-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Net management.
package net

import (
	"errors"
	"fmt"
	"github.com/dedeme/QMarket/data/cts"
	"github.com/dedeme/QMarket/data/nick"
	"github.com/dedeme/QMarket/data/quote"
	"github.com/dedeme/QMarket/data/server"
	"github.com/dedeme/QMarket/db/nicksTb"
	"github.com/dedeme/QMarket/db/quotesDb"
	"github.com/dedeme/QMarket/db/serversTb"
	"github.com/dedeme/golib/file"
	"github.com/dedeme/golib/sys"
	"regexp"
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

	panic("'" + cmd + "' is no a valid program to read web pages")
}

// Regularize number.
func toNumber(isIso bool, n string) (nn float64, err error) {
	if isIso {
		n = strings.ReplaceAll(strings.ReplaceAll(n, ".", ""), ",", ".")
	} else {
		n = strings.ReplaceAll(n, ",", "")
	}

	nn, e := strconv.ParseFloat(n, 64)
	if e != nil {
		if n == "" || strings.HasPrefix(n, "-") {
			nn = 0
			return
		}
		err = errors.New(fmt.Sprintf("Wrong number reading server (%v)", n))
		return
	}
	if nn < 0 {
		err = errors.New(fmt.Sprintf("Negative quote reading server (%v)", nn))
		return
	}

	return
}

// Regularize date.
func mkDate(d string, sep string, isIso bool) (dd string, err error) {
	readYahoo := func() (yd string, yerr error) {
		months := []string{
			"ene", "feb", "mar", "abr", "may", "jun",
			"jul", "ago", "sept", "oct", "nov", "dic",
		}
		parts := strings.Split(d, " ")
		if len(parts) != 3 {
			yerr = errors.New(fmt.Sprintf("Wrong Yahoo date (%v)", d))
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
			return
		}
		yerr = errors.New(fmt.Sprintf("Wrong Yahoo date (%v)", d))
		return
	}

	if sep == "*" {
		dd, err = readYahoo()
		return
	}

	parts := strings.Split(d, sep)
	if len(parts) != 3 {
		err = errors.New(fmt.Sprintf("Wrong date reading server (%v)", d))
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
	err = errors.New(fmt.Sprintf("Wrong date reading server (%v)", d))
	return
}

// Returns html rest after group of start marks.
func goStart(html string, marks []string) (rest string, err error) {
	if len(marks) == 0 {
		err = errors.New("Start marks are missing.")
		return
	}
	for _, e := range marks {
		ix := strings.Index(html, e)
		if ix == -1 {
			err = errors.New("Mark '" + e + "'not found reading web page.")
			return
		}
		html = html[ix+len(e):]
	}
	rest = html
	return
}

// Returns html text and rest after group of end marks or ok=false if the group
// is not found.
func goEnd(html string, marks []string) (text, rest string, err error) {
	if len(marks) == 0 {
		err = errors.New("There are no end marks in web page.")
	}

	for {
		mk0 := marks[0]

		ix := strings.Index(html, mk0)
		if ix == -1 {
			err = errors.New("Mark '" + mk0 + "'not found reading web page.\n" + html)
			return
		}
		text = text + html[:ix]
		rest, err = goStart(html[ix:], marks)
		if err == nil {
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
) (table [][]string, warnings []error, err error) {
	getHtml := func() (html string, herr error) {
		url := conf.Url()
		if code != "" {
			url = strings.ReplaceAll(url, "${code}", code)
		}
		html = download(conf.Cmd(), url)

		if html == "" {
			herr = errors.New(fmt.Sprintf("Inet error reading '%v'", url))
			return
		}

		if strings.TrimSpace(conf.Regex()) != "" {
			exps := strings.Split(conf.Regex(), "\n")
			for _, l := range exps {
				ps := strings.Split(strings.TrimSpace(l), "|x|")
				if len(ps) == 2 {
					exp := strings.TrimSpace(ps[0])
					if exp != "" {
						re := regexp.MustCompile(exp)
						html = re.ReplaceAllString(html, strings.TrimSpace(ps[1]))
					}
				}
			}
		}

		html, herr = goStart(html, strings.Split(conf.TableStart(), "|"))
		if herr != nil {
			return
		}

		html, _, herr = goEnd(html, strings.Split(conf.TableEnd(), "|"))
		return
	}

	html, err := getHtml()
	if err != nil {
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
		err = errors.New(fmt.Sprintf(
			"Length of start cells != length of end cells (Start: %v, End: %v)",
			len(cellsStart), len(cellsEnd),
		))
		return
	}
	if len(cellsStart) == 0 {
		err = errors.New("Length of start cells == 0")
		return
	}

	for {
		var werr error
		var row []string
		html, werr = goStart(html, rowStart)
		if werr != nil {
			break
		}

		var htmlRow string
		htmlRow, html, werr = goEnd(html, rowEnd)
		if werr != nil {
			warnings = append(warnings, werr)
			break
		}

		for i := 0; i < len(cellsStart); i++ {
			htmlRow, werr = goStart(htmlRow, cellsStart[i])
			if werr != nil {
				werr = nil
				break
			}
			var text string
			text, htmlRow, werr = goEnd(htmlRow, cellsEnd[i])
			if werr != nil {
				warnings = append(warnings, werr)
				break
			}
			row = append(row, strings.TrimSpace(text))
		}
		if werr == nil {
			table = append(table, row)
		}
	}

	return
}

// If some error happends which allows to go on reading, a warning is added.
func readDaily(
	conf *server.ConfT,
) (entries []*dailyEntryT, warnings []error, err error) {
	table, warnings, err := read(conf, "")
	if err != nil {
		return
	}
	ixC := strings.Index(conf.FieldsType(), "C")
	if ixC == -1 {
		err = errors.New("Code field is missing in server configuration")
		return
	}
	ixQ := strings.Index(conf.FieldsType(), "Q")
	if ixQ == -1 {
		err = errors.New("Quote field is missing in server configuration")
		return
	}

	for _, row := range table {
		close, werr := toNumber(conf.IsIsoNumber(), row[ixQ])
		if werr == nil {
			entries = append(entries, &dailyEntryT{row[ixC], close})
		} else {
			warnings = append(
				warnings,
				errors.New("Fail reading quote of '"+row[ixC]+"'. Quote sets to 0."),
			)
		}
	}

	if len(table) == 0 {
		err = errors.New("Quotes table read from web page is empty")
	}
	return
}

// If some error happends which allows to go on reading, a warning is added.
func readHistoric(
	conf *server.ConfT, code string,
) (entries []*historicEntryT, warnings []error, err error) {
	table, warnings, err := read(conf, code)
	if err != nil {
		return
	}

	ixD := strings.Index(conf.FieldsType(), "D")
	if ixD == -1 {
		err = errors.New("Date field is missing in server configuration")
		return
	}
	ixO := strings.Index(conf.FieldsType(), "O")
	if ixO == -1 {
		err = errors.New("Open field is missing in server configuration")
		return
	}
	ixC := strings.Index(conf.FieldsType(), "C")
	if ixC == -1 {
		err = errors.New("Close field is missing in server configuration")
		return
	}
	ixX := strings.Index(conf.FieldsType(), "X")
	if ixX == -1 {
		err = errors.New("Maximum field is missing in server configuration")
		return
	}
	ixN := strings.Index(conf.FieldsType(), "N")
	if ixN == -1 {
		err = errors.New("Minimum field is missing in server configuration")
		return
	}
	ixV := strings.Index(conf.FieldsType(), "V")
	if ixV == -1 {
		err = errors.New("Volume field is missing in server configuration")
		return
	}

	for _, row := range table {
		date, werr := mkDate(row[ixD], conf.DateSeparator(), conf.IsIsoDate())
		if werr != nil {
			warnings = append(warnings, werr)
			continue
		}
		open, werr := toNumber(conf.IsIsoNumber(), row[ixO])
		if werr != nil {
			warnings = append(warnings, werr)
			continue
		}
		close, werr := toNumber(conf.IsIsoNumber(), row[ixC])
		if werr != nil {
			warnings = append(warnings, werr)
			continue
		}
		max, werr := toNumber(conf.IsIsoNumber(), row[ixX])
		if werr != nil {
			warnings = append(warnings, werr)
			continue
		}
		min, werr := toNumber(conf.IsIsoNumber(), row[ixN])
		if werr != nil {
			warnings = append(warnings, werr)
			continue
		}
		volf, werr := toNumber(conf.IsIsoNumber(), row[ixV])
		if werr != nil {
			warnings = append(warnings, werr)
			continue
		}
		vol := int(volf)
		entries = append(entries, &historicEntryT{date, open, close, max, min, vol})
	}

	if len(table) == 0 {
		err = errors.New("Quotes table read from web page is empty")
	}
	return
}

// Read one daily server
func OneServerReadDaily(
	sv *server.T,
) (qvs []*nick.QvalueT, warnings []error, err error) {
	conf, ok := sv.DailyConf()
	if !ok {
		err = errors.New(fmt.Sprintf(
			"Server %v: Daily configuration not defined", sv.Name(),
		))
		return
	}

	table, warnings, err := readDaily(conf)
	if err != nil {
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
			nk, ok := nicksTb.Read().NickFromId(nickId)
			nkName := strconv.Itoa(nickId)
			if ok {
				nkName = nk.Name()
			}
			warnings = append(warnings, errors.New(fmt.Sprintf(
				"Server '%v' has not defined code for '%v'", sv.Name(), nkName,
			)))
		}
		qvs = append(qvs, nick.NewQvalue(nickId, value))
	}

	return
}

// Reads daily quotes.
//
// Companies without server code or whose code is not found generate a
// quote value of '0' and produce a log error.
//   sv: Web server.
// If some error happends which allows to go on reading, a warning is added.
//    -------
//    Returns:
//      qvs     : Quotes-Value read.
//      warnings: List of warnigs. If "err!=nil" its value is undetermined.
//      error   : error found.
func ServerReadDaily(
	sv *server.T,
) (qvs []*nick.QvalueT, warnings []error, err error) {

	svsTb := serversTb.Read()
	servers, sel := svsTb.DailyList()
	qvs, warnings, err = OneServerReadDaily(servers[sel])
	if err != nil {
		for _, sv := range servers {
			qvs, warnings, err = ServerReadDaily(sv)
			if err == nil {
				break
			}
		}
		if err != nil {
			err = errors.New("net/ServerReadDaily: No server can read daily quotes")
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
//      warnings: List of warnigs. If "err!=nil" its value is undetermined.
//      error   : error found.
func ServerReadHistoric(
	sv *server.T, nk *nick.T,
) (qs []*quote.T, warnings []error, err error) {
	nickId := nk.Id()

	tryRead := func(
		times int, conf *server.ConfT, code string,
	) (qs []*quote.T, terr error, twarnings []error) {
		for i := 0; i < times; i++ {
			var entries []*historicEntryT
			entries, twarnings, terr = readHistoric(conf, code)
			if terr != nil {
				continue
			}
			if len(entries) < cts.HistoricMinimumEntries {
				if i+1 >= times {
					terr = errors.New(fmt.Sprintf(
						"Historic entries of nick '%v' in server '%v' are less than "+
							"%v (%v)",
						nk.Name(), sv.Name(), cts.HistoricMinimumEntries, len(entries),
					))
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
		err = errors.New(fmt.Sprintf(
			"Server %v: Historic configuration not defined", sv.Name(),
		))
		return
	}

	for _, nkCd := range sv.Codes() {
		if nkCd.NickId() == nickId {
			code, cOk := nkCd.Code()
			if !cOk {
				err = errors.New(fmt.Sprintf(
					"Server '%v' has not defined code for '%v'",
					sv.Name(), nk.Name(),
				))
				return
			}
			qs, err, warnings = tryRead(2, conf, code)
			if err == nil {
				for i, e := range warnings {
					warnings[i] = errors.New("[" + nk.Name() + "] " + e.Error())
				}
			}
			return
		}
	}

	err = errors.New(fmt.Sprintf(
		"Code of '%v' is missing in '%v'",
		nk.Name(), sv.Name(),
	))
	return
}

// Test daily configuration and returns 'true' if the result was ok.v
//    id: Server identifier.
//    -------
//    Returns:
//      warnings: List of warnigs. If "err!=nil" its value is undetermined.
//      error   : error found.
func TestDailyConf(id int) (warnings []error, err error) {
	for _, sv := range serversTb.Read().List() {
		if sv.Id() == id {
			var nickCloses []*nick.QvalueT
			nickCloses, warnings, err = OneServerReadDaily(sv)
			if err == nil && len(nickCloses) == len(sv.Codes()) {
				// Show test result ----------------------------
				/*
					codes := sv.Codes()
					for i := 0; i < len(nickCloses); i++ {
						nick, _ := nicksTb.GetNick(lk, codes[i].NickId())
						fmt.Println(nick, " = ", nickCloses[i])
					}
				*/
				// ---------------------------------------------

				return
			}
			if len(nickCloses) != len(sv.Codes()) {
				err = errors.New("Some companies have not been read")
			}
			return
		}
	}
	err = errors.New("Server identifier not found")
	return
}

// Test historic configuration.
//    id  : Server identifier.
//    nick: Company nick.
//    -------
//    Returns:
//      warnings: List of warnigs. If "err!=nil" its value is undetermined.
//      error   : error found.
func TestHistoricConf(id int, nk *nick.T) (warnings []error, err error) {
	for _, sv := range serversTb.Read().List() {
		if sv.Id() == id {
			_, warnings, err = ServerReadHistoric(sv, nk)
			return
		}
	}
	err = errors.New("Server identifier not found")
	return
}

// Updates historic quotes of "nickId".
//    nickName: company nick.
//    -------
//    Returns:
//      warnings: List of warnigs. If "err!=nil" its value is undetermined.
//      error   : error found.
func UpdateHistoric(nk *nick.T) (warnings []error, err error) {
	head := "net/UpdateHistoric [" + nk.Name() + "]:"
	readQs := func(nkName string) (qs []*quote.T, rerr error) {
		qs, err := quotesDb.Read(nkName)
		if err != nil {
			err = errors.New(head + "\n" + err.Error())
		}
		return
	}

	var oldQs []*quote.T
	oldQs, err = readQs(nk.Name())
	if err != nil {
		return
	}

	nksTb := nicksTb.Read()
	nkModelId, ok := nksTb.Model()
	if !ok {
		err = errors.New(head + " Nick model not defined")
		return
	}
	nkModel, ok := nksTb.NickFromId(nkModelId)
	if !ok {
		err = errors.New(fmt.Sprintf(
			head+" Nick model id '%v' not found", nkModelId,
		))
		return
	}

	var modelQs []*quote.T
	if nkModelId != nk.Id() {
		modelQs, err = readQs(nkModel.Name())
		if err != nil {
			return
		}
	}

	svsTb := serversTb.Read()
	servers, sel := svsTb.HistoricList()
	newQs, warns, err := ServerReadHistoric(servers[sel], nk)
	if err != nil {
		for _, sv := range servers {
			newQs, warns, err = ServerReadHistoric(sv, nk)
			if err == nil {
				break
			}
		}
		if err != nil {
			err = errors.New(fmt.Sprintf(
				"net/UpdateHistoric: No server can read quotes of %v", nk.Name(),
			))
			return
		}
	}
	warnings = append(warnings, warns...)

	qs, serrors := quote.Merge(modelQs, newQs, oldQs)
	for _, w := range serrors {
		warnings = append(warnings, errors.New("["+nk.Name()+"] "+w.Error()))
	}

	ok = quotesDb.Write(nk.Name(), qs)
	if !ok {
		err = errors.New(fmt.Sprintf(head+" File %v.tb not found", nk.Name()))
	}

	return
}
