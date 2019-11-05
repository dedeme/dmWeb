// Copyright 28-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "quotes_tests.h"
#include <assert.h>
#include "dmc/match.h"
#include "dmc/date.h"
#include "io/quotes.h"
#include "DEFS.h"

void quotes_tests (void) {
  puts("Quotes tests:");

  time_t now = date_now();
  time_t start = date_new(1, 1, date_year(now) - 1);
  // Arr<char>
  Arr *best_nicks = quotes_bests(start);
  // Arr<char>
  Arr *nicks = arr_new();
  EACH_IX(best_nicks, char, nk, ix) {
    if (ix == PLAYERS_PER_LEAGUE)
      break;
    arr_push(nicks, nk);
  }_EACH

  MatchLeague *league = match_league(1, PLAYERS_PER_LEAGUE);
  MatchRound *r = match_next_round(league);
  MatchRsRound *rs = opt_nget(
    quotes_results_opt(date_new(4, 8, 2019), nicks, r)
  );
  assert(!rs);

  rs = opt_nget(
    quotes_results_opt(date_new(5, 8, 2019), nicks, r)
  );
  assert(rs);
  match_add(league, rs);

  /*
  EACH(match_maches_results(league, 0), MatchPR, pr) {
    MatchPlayers *p = matchPR_players(pr);
    MatchResult *r = matchPR_result(pr);
    printf(
      "%s-%s: %d:%.4f\n",
      (char *)arr_get(nicks, matchPlayers_up(p)),
      (char *)arr_get(nicks, matchPlayers_down(p)),
      matchResult_rs(r), matchResult_points(r)
    );
  }_EACH

  // Arr<Arr<char>>
  Arr *qs = arr_new_from(
    arr_new_from("MAS", "19.7200", "20.3000", NULL),
    arr_new_from("APPS", "12.0300", "12.5700", NULL),
    arr_new_from("FDR", "10.7400", "11.2000", NULL),
    arr_new_from("REE", "17.5750", "17.6450", NULL),
    arr_new_from("ENC", "3.3940", "3.4140", NULL),
    arr_new_from("ACS", "34.2100", "35.5900", NULL),
    arr_new_from("SLR", "5.3350", "5.4850", NULL),
    arr_new_from("ZOT", "6.3050", "6.2300", NULL),
    arr_new_from("CLNX", "34.0200", "34.1400", NULL),
    arr_new_from("MRL", "11.8400", "12.1900", NULL),
    arr_new_from("NHH", "4.3780", "4.4280", NULL),
    arr_new_from("GRF", "28.2200", "28.9200", NULL),
    arr_new_from("CASH", "1.7640", "1.8000", NULL),
    arr_new_from("TUB", "2.7900", "2.7800", NULL),
    arr_new_from("AMS", "68.7400", "69.9400", NULL),
    arr_new_from("OHL", "1.0620", "1.0840", NULL),
    arr_new_from("PSG", "4.0700", "4.1100", NULL),
    arr_new_from("MTS", "12.7800", "13.3560", NULL),
    arr_new_from("COL", "10.2000", "10.3100", NULL),
    arr_new_from("IAG", "4.8390", "4.8780", NULL),
    NULL
  );

  RANGE0(i, 10) {
    // Arr<char>
    Arr *a1 = arr_get(qs, i * 2);
    // Arr<char>
    Arr *a2 = arr_get(qs, i * 2 + 1);
    double af1 = js_rd(arr_get(a1, 1));
    double bf1 = js_rd(arr_get(a1, 2));
    double af2 = js_rd(arr_get(a2, 1));
    double bf2 = js_rd(arr_get(a2, 2));

    printf(
      "%s-%s:%.4f\n",
      (char *)arr_get(a1, 0), (char *)arr_get(a2,0),
      (af1 - bf1) / bf1 - (af2 - bf2) / bf2
    );
  }_RANGE
  */

  /*
  rs = opt_nget(quotes_last_results_opt(nicks, r));
  if (rs) {
    match_add(league, rs);
    EACH(match_maches_results(league, 0), MatchPR, pr) {
      MatchPlayers *p = matchPR_players(pr);
      MatchResult *r = matchPR_result(pr);
      printf(
        "%s-%s: %d:%.4f\n",
        (char *)arr_get(nicks, matchPlayers_up(p)),
        (char *)arr_get(nicks, matchPlayers_down(p)),
        matchResult_rs(r), matchResult_points(r)
      );
    }_EACH
  }
  */
  puts("    Finished");
}

