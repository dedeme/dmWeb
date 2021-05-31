// Copyright 23-May-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package sighter

import (
	"math/rand"
)

type T interface {
	Id() string
	Level() int
	Sights() int
	Inc() T
	Reset() T
}

func resetSights(sighters []T) (newSighters []T) {
	for _, sg := range sighters {
		newSighters = append(newSighters, sg.Reset())
	}
	return
}

func incSight(sighters []T, s T) (newSighters []T, news T) {
	for _, sg := range sighters {
		if sg.Id() == s.Id() {
			news = sg.Inc()
			newSighters = append(newSighters, news)
		} else {
			newSighters = append(newSighters, sg)
		}
	}
	return
}

func next(
	isFirst bool,
	getGroups func() []string,
	read func(string) []T,
	write func(string, []T),
) (group string, s T) {
	n := 0
	for _, g := range getGroups() {
		for _, sg := range read(g) {
			if sg.Sights() < sg.Level() {
				n++
			}
		}
	}

	if n == 0 {
		if isFirst {
			for _, g := range getGroups() {
				write(g, resetSights(read(g)))
			}
			return next(false, getGroups, read, write)
		}
		panic("No available picture")
	}

	sel := rand.Intn(n)

	var sighters []T
	end := false
	n = 0
	for _, g := range getGroups() {
		group = g
		sighters = read(group)
		for _, e := range sighters {
			if e.Sights() < e.Level() {
				if n == sel {
					end = true
					s = e
					break
				} else {
					n++
				}
			}
		}
		if end {
			break
		}
	}

	if s == nil {
		panic("New picture not found")
	}

	sighters, s = incSight(sighters, s)
	write(group, sighters)
	return
}

// Returns the next sighter
func Next(
	getGroups func() []string,
	read func(string) []T,
	write func(string, []T),
) (group string, s T) {
	return next(true, getGroups, read, write)
}
