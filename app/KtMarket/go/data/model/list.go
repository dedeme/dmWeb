// Copyright 08-Jun-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

// Model data.
package model

func List() []*T {
	return []*T{
		newQfix(),
		newQmob(),
		newAppr(),
	}
}
