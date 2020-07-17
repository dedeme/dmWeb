// Copyright 09-Jul-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Web source data
package webSource

import (
	"github.com/dedeme/News/data/webPage"
)

type T struct {
	id    string
	pages []*webPage.T
}

// Returns source identifier.
func (s *T) Id() string {
	return s.id
}

// Returns web pages to search.
func (s *T) Pages() []*webPage.T {
	return s.pages
}

// Sources to search.
func List() []*T {
	return []*T{
		&T{
			id: "ElDiario",
			pages: []*webPage.T{
				&webPage.T{
					SourceId: "ElDiario",
					Url:      "https://www.eldiario.es",
					Root:     "https://www.eldiario.es",
				},
			},
		},
		&T{
			id: "ElConfidencial",
			pages: []*webPage.T{
				&webPage.T{
					SourceId: "ElConfidencial",
					Url:      "https://www.elconfidencial.com",
					Root:     "",
				},
			},
		},
		&T{
			id: "Publico",
			pages: []*webPage.T{
				&webPage.T{
					SourceId: "Publico",
					Url:      "https://www.publico.es",
					Root:     "https://www.publico.es",
				},
			},
		},
		&T{
			id: "Meneame",
			pages: []*webPage.T{
				&webPage.T{
					SourceId: "Meneame",
					Url:      "https://www.meneame.net",
					Root:     "",
				},
				&webPage.T{
					SourceId: "Meneame",
					Url:      "https://www.meneame.net/?page=2",
					Root:     "",
				},
			},
		},
	}
}
