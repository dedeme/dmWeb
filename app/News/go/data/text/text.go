// Copyright 06-Jul-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Processing text utilities.
package text

import (
	"strconv"
	"strings"
	"unicode"
)

const skippedWords = " A AL CON DE DEL EL EN ES ERES HAN HAY HEMOS " +
	"LA LOS LAS LO LE NI NO NOS O POR QUE SE SEA SER SIN SON SU " +
	"TE TU UN UNOS UNA UNAS Y "

func isValid(rn rune) bool {
	return unicode.IsDigit(rn) || unicode.IsLetter(rn) || string(rn) == "."
}

func mapWord(s string) string {
	if _, err := strconv.ParseFloat(s, 32); err == nil {
		return "0"
	}
	if strings.HasSuffix(s, "...") {
		return "..."
	} else if strings.HasSuffix(s, ".") {
		return strings.TrimRight(s, ".")
	}
	s = strings.ReplaceAll(s, "Á", "A")
	s = strings.ReplaceAll(s, "É", "E")
	s = strings.ReplaceAll(s, "Í", "I")
	s = strings.ReplaceAll(s, "Ó", "O")
	s = strings.ReplaceAll(s, "Ú", "U")
	s = strings.ReplaceAll(s, "Ü", "U")
	if strings.Contains(skippedWords, " "+s+" ") {
		return " "
	}
	return s
}

// Reads word list. It makes next changes:
//  - Text is 'uppecased'
//  - Numbers are converted to "0".
//  - Words ended in '...' are replaced by word + "..."
func WordsRead(tx string) []string {
	rd := strings.NewReader(strings.ToUpper(tx))
	var r []string
	var bf strings.Builder
	for {
		if rn, _, err := rd.ReadRune(); err == nil {
			if isValid(rn) {
				bf.WriteRune(rn)
			} else if bf.Len() > 0 {
				s := mapWord(bf.String())
				if s != " " {
					r = append(r, s)
				}
				bf.Reset()
			}
			continue
		}
		break
	}
	if bf.Len() > 0 {
		s := mapWord(bf.String())
		if s != " " {
			r = append(r, s)
		}
	}
	return r
}
