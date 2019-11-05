// Copyright 25-Jul-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "Mark.h"
#include "Match.h"

/* .
Mark
  nick: char *
  points: int
  dif: double
*/

/*--*/

struct Mark_Mark {
  char *nick;
  int points;
  double dif;
};

Mark *mark_new (char *nick, int points, double dif) {
  Mark *this = MALLOC(Mark);
  this->nick = nick;
  this->points = points;
  this->dif = dif;
  return this;
}

char *mark_nick (Mark *this) {
  return this->nick;
}

int mark_points (Mark *this) {
  return this->points;
}

double mark_dif (Mark *this) {
  return this->dif;
}

/*--*/

// Arr[char]
Arr *mark_ranking (DataLeague *league) {
  // Arr[DataRoundResult]
  Arr *lresults = dataLeague_results(league);
  int nnicks = arr_size(dataLeague_nicks(league));
  int nplayers = nnicks + nnicks % 2;
  // Arr[Arr[Mach]]
  Arr *rounds = match_rounds(nplayers);

  // Arr[Mark]
  Arr *marks = arr_new();
  for (int nk_ix = 0; nk_ix < nnicks; ++nk_ix) {
    char *nick = arr_get(dataLeague_nicks(league), nk_ix);
    int points = 0;
    double dif = 0;
    for (int round_ix = 0; round_ix < nplayers - 1; ++round_ix) {
      // Arr[Match]
      Arr *matches = arr_get(rounds, round_ix);
      // Arr[DataMatchResult]
      Arr *round_results = dataRoundResult_results(arr_get(lresults, round_ix));
      for (int matches_ix = 0; matches_ix < arr_size(matches); ++matches_ix) {
        Match *m = arr_get(matches, matches_ix);
        if (match_up(m) == nk_ix || match_down(m) == nk_ix) {
          DataMatchResult *mrs = arr_get(round_results, matches_ix);
          int rs = dataMatchResult_result(mrs);
          if (rs == -1) break;
          dif += dataMatchResult_dif(mrs);
          if (rs == 1) {
            if (match_up(m) == nk_ix) points += 2;
          } else if (rs == 2) {
            if (match_down(m) == nk_ix) points += 2;
          } else {
            ++points;
          }
          break;
        }
      }
    }
    arr_push(marks, mark_new(nick, points, dif));
  }

  int fsort (Mark *m1, Mark *m2) {
    return mark_points(m1) == mark_points(m2)
      ? mark_dif(m1) < mark_dif(m2)
      : mark_points(m1) < mark_points(m2)
    ;
  }
  arr_sort(marks, (FCMP)fsort);

  char *fmap (Mark *m) { return mark_nick(m); }
  return it_to(it_map(it_from(marks), (FCOPY)fmap));
}

