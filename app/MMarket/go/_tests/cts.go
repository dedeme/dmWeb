// Copyright 12-Apr-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package _tests

import (
	"fmt"
)

const fail = "\nActual  : false\nExpected: true\n"

func eqs(actual, expected string) string {
	if actual != expected {
		return fmt.Sprintf("\nActual  : %v\nExpected: %v\n", actual, expected)
	}
	return ""
}

func eqi(actual, expected int64) string {
	if actual != expected {
		return fmt.Sprintf("\nActual  : %v\nExpected: %v\n", actual, expected)
	}
	return ""
}

func eqf(actual, expected float64) string {
	if actual > expected+0.00000001 || actual < expected-0.00000001 {
		return fmt.Sprintf("\nActual  : %v\nExpected: %v\n", actual, expected)
	}
	return ""
}
