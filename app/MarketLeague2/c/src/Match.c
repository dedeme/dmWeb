// Copyright 24-Jul-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "Match.h"
#include "dmc/Iarr.h"

/* .
Match:
  up: int
  down: int
*/

/*--*/

struct Match_Match {
  int up;
  int down;
};

Match *match_new (int up, int down) {
  Match *this = MALLOC(Match);
  this->up = up;
  this->down = down;
  return this;
}

int match_up (Match *this) {
  return this->up;
}

int match_down (Match *this) {
  return this->down;
}

/*--*/

// Returns Arr[Arr[Match]]
Arr *match_rounds (int players) {
  if (players % 2)
    EXC_ILLEGAL_ARGUMENT(
      "players", "Even value", str_f("Odd value (%d)", players)
    )

  Iarr *a = iarr_new();
  RANGE0(i, players)
    iarr_push(a, i);
  _RANGE

  // Arr[Arr[Match]]
  Arr *r = arr_new();
  int players1 = players - 1;
  int mid = players / 2;
  REPEAT(players1)
    // Arr[Match]
    Arr *matches = arr_new();
    RANGE0(i, mid)
      arr_push(
        matches,
        match_new(iarr_get(a, i), iarr_get(a, players1 - i))
      );
    _RANGE

    arr_push(r, matches);

    int last = iarr_get(a, players1);
    int i;
    for (i = players1; i > 1; --i) {
      iarr_set(a, i, iarr_get(a, i - 1));
    }
    iarr_set(a, 1, last);
  _REPEAT

  return r;
}
