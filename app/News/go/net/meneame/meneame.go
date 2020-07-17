// Copyright 09-Jul-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Meneame reader
package meneame

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

func mapSource(source string) string {
  if source == "eldiario.es" {
    return "ElDiario"
  }
  return source
}

func newRead(
	wpg *webPage.T, pg string,
) (e *newsEntry.T, rest string) {

	e = nil
	p := strings.Index(pg, "<h2>")
	if p == -1 {
		return
	}
	pg = pg[p+4:]
	rest = pg
	p = strings.Index(pg, "\"")
	if p == -1 {
		return
	}
	pg = pg[p+1:]
	rest = pg
	p = strings.Index(pg, "\"")
	if p == -1 {
		return
	}
  url := pg[:p]
	pg = pg[p+1:]
	rest = pg


	p = strings.Index(pg, ">")
	if p == -1 {
		return
	}
	pg = pg[p+1:]
	rest = pg
	p = strings.Index(pg, "<")
	if p == -1 {
		return
	}
  text := strings.TrimSpace(html.UnescapeString(pg[:p]))
  pg = pg[p+1:]
	rest = pg
	p = strings.Index(pg, ">  por <")
	if p == -1 {
		return
	}
  pg = pg[p+7:]
  rest = pg
	p = strings.Index(pg, ">")
	if p == -1 {
		return
	}
  pg = pg[p+1:]
	rest = pg
	p = strings.Index(pg, "<")
	if p == -1 {
		return
	}
  author := strings.TrimSpace(html.UnescapeString(pg[:p]))
  pg = pg[p+1:]
	rest = pg
	p = strings.Index(pg, ">  a <")
	if p == -1 {
		return
	}
  pg = pg[p+6:]
  rest = pg
	p = strings.Index(pg, ">")
	if p == -1 {
		return
	}
  pg = pg[p+1:]
	rest = pg
	p = strings.Index(pg, "<")
	if p == -1 {
		return
	}
  source := mapSource(strings.TrimSpace(html.UnescapeString(pg[:p])))
  pg = pg[p+1:]
	rest = pg

	e = &newsEntry.T{
		Date:     date.Now(),
		Text:     text,
		Source:   source,
		Author:   author,
		Url:      url,
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
