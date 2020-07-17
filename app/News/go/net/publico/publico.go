// Copyright 13-Jul-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Publico
package publico

import (
	"fmt"
	"github.com/dedeme/News/data/ev"
	"github.com/dedeme/News/data/newsEntry"
	"github.com/dedeme/News/data/webPage"
	"github.com/dedeme/News/net/reader"
	"github.com/dedeme/golib/date"
	"html"
	"strings"
)

func pgStart(pg string) int {
	return 0
}

func newRead(
	wpg *webPage.T, pg string,
) (e *newsEntry.T, rest string) {
	source := wpg.SourceId
	root := wpg.Root
	author := source

	e = nil
	p := strings.Index(pg, "<span class=\"title\">")
	if p == -1 {
		return
	}
	pg = pg[p+20:]
	rest = pg
	p = strings.Index(pg, " href=\"")
	if p == -1 {
		return
	}
	pg = pg[p+7:]
	p = strings.Index(pg, "\"")
	if p == -1 {
		return
	}
	url := pg[:p]
	pg = pg[p:]
	p = strings.Index(pg, ">")
	if p == -1 {
		return
	}
	pg = pg[p+1:]
	p = strings.Index(pg, "<")
	text := strings.TrimSpace(html.UnescapeString(pg[:p]))
	rest = pg[p+1:]

	p = strings.Index(pg, "<span class=\"author\"><a")
	if p != -1 {
		pg = pg[p+23:]
		p = strings.Index(pg, ">")
		if p != -1 {
			pg = pg[p+1:]
			p = strings.Index(pg, "<")
			if p != -1 {
				author = pg[:p]
				rest = pg[p+1:]
			}
		}
	}

	if strings.HasPrefix(url, "http") {
		return
	}

	e = &newsEntry.T{
		Date:     date.Now(),
		Text:     text,
		Source:   source,
		Author:   author,
		Url:      root + url,
		Seval:    0.5,
		Aeval:    0.5,
		Weval:    0.5,
		UserEval: ev.NONE,
	}

	return
}

func Read(wpg *webPage.T) []*newsEntry.T {
	url := wpg.Url
	var r []*newsEntry.T
	pg := reader.ReadPage(url)
	start := pgStart(pg)
	if start < 0 {
		panic(fmt.Sprintf("Start of page not found (%v)", url))
	}

	pg = pg[start+1:]
	var e *newsEntry.T
	for {
		e, pg = newRead(wpg, pg)
		if pg == "" {
			break
		}
		if e != nil {
			r = append(r, e)
		}
	}

	return r
}
