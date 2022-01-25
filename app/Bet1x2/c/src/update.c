// Copyright 24-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "update.h"
#include <stdlib.h>
#include "dmc/err.h"
#include "dmc/str.h"
#include "dmc/AInt.h"
#include "net/as.h"
#include "net/marca.h"
#include "net/sportium.h"
#include "data/cts.h"
#include "db/results.h"
#include "db/teams.h"
#include "db/points.h"
#include "db/bets.h"

static void update_results (void) {
  AKchar *teams = mchar_to_array(cts_teams());
  int teams_size = aKchar_size(teams);

  AAOResult *as_results = as_read();
  AAOResult *marca_results = marca_read();

  if (aAOResult_size(as_results) != teams_size) {
    FAIL(str_f("Bad number of 'as_results' rows. Expected %d, found %d.",
      teams_size, aAOResult_size(as_results)));
  }

  for (int row = 0; row < teams_size; ++row) {
    AOResult *as_row = aAOResult_get(as_results, row);
    AOResult *marca_row = aAOResult_get(marca_results, row);
    for (int col = 0; col < teams_size; ++col) {
      if (
        !oResult_eq(aOResult_get(as_row, col), aOResult_get(marca_row, col))
      ) {
        FAIL(str_f(
          "Results for %s - %s, does not match:\nas: %s, marca: %s.",
          aKchar_get(teams, row)->v, aKchar_get(teams, col)->v,
          oResult_to_js(aOResult_get(as_row, col)),
          oResult_to_js(aOResult_get(marca_row, col))
        ));
      }
    }
  }

  results_write(as_results);
}

static void evaluate (int *played, int *points, AAOResult *rss, int team) {
  int py = 0;
  int pts = 0;

  for (int i = 0; i < aAOResult_size(rss); ++i) {
    Result *r = oResult_nsome(aOResult_get(aAOResult_get(rss, i), team));
    if (r) {
      ++py;
      pts += r->home == r->out ? 1 : r->out > r->home ? 2 : 0;
    }
  }

  AOResult *rs = aAOResult_get(rss, team);
  for (int i = 0; i < aOResult_size(rs); ++i) {
    Result *r = oResult_nsome(aOResult_get(rs, i));
    if (r) {
      ++py;
      pts += r->home == r->out ? 1 : r->home > r->out ? 2 : 0;
    }
  }

  *played = py;
  *points = pts;
}

static void update_points (void) {
  AAOResult *prss = NULL;
  AKchar *pteams= NULL;
  char *pyear = str_f("%d", atoi(cts_year()) - 1);
  /**/int findex (char *f) { return str_eq(f, pyear); }
  if (achar_index(teams_years(), findex) != -1) {
    prss = results_read(pyear);
    pteams = teams_read(pyear);
  }

  AAOResult *rss = results_read(cts_year());
  AKchar *teams= teams_read(cts_year());
  AAOResult *points_tb = points_read(cts_year());
  int midPlays = aKchar_size(teams) - 1;
  int totalPlays = midPlays * 2;

  AInt *tpoints = aInt_new();
  for (int team = 0; team < aKchar_size(teams); ++team) {
    int played = 0;
    int points = 0;
    evaluate(&played, &points, rss, team);

    if (prss && pteams && played < midPlays) {
      int ppond = (midPlays - played) * 2;
      int cpond = totalPlays - ppond;
      /**/int findex2 (Kchar *e) {
      /**/  return str_eq(e->k, aKchar_get(teams, team)->k);
      /**/}
      int pteam = aKchar_index(pteams, findex2);
      if (pteam != -1) {
        int pplayed = 0;
        int ppoints = 0;
        evaluate(&pplayed, &ppoints, prss, pteam);

        points = (ppoints * ppond + points * cpond) / totalPlays;
      }
    }

    aInt_push(tpoints, points);
  }

  for (int row = 0; row < aKchar_size(teams); ++row) {
    for (int col = 0; col < aKchar_size(teams); ++col) {
      if (row == col) continue;

      Result *r = oResult_nsome(
        aOResult_get(aAOResult_get(rss, row), col)
      );
      Result *p = oResult_nsome(
        aOResult_get(aAOResult_get(points_tb, row), col)
      );

      if (!p || !r) {
        Result *r = result_new(aInt_get(tpoints, row), aInt_get(tpoints, col));
        aOResult_set(aAOResult_get(points_tb, row), col, oResult_mk_some(r));
      }
    }
  }

  points_write(points_tb);
}

static void update_bets (void) {
  AAOBet *pbets = bets_read(cts_year());

  AKchar *teams = mchar_to_array(cts_teams());
  int teams_size = aKchar_size(teams);
  AAOBet *sportium_bets = sportium_read();
  if (aAOBet_size(sportium_bets) != teams_size) {
    FAIL(str_f("Bad number of 'as_results' rows. Expected %d, found %d.",
      teams_size, aAOBet_size(sportium_bets)));
  }

  int changed = 0;
  for (int row = 0; row < teams_size; ++row) {
    AOBet *pbets_row = aAOBet_get(pbets, row);
    AOBet *sportium_row = aAOBet_get(sportium_bets, row);
    for (int col = 0; col < teams_size; ++col) {
      Bet *pbet = oBet_nsome(aOBet_get(pbets_row, col));
      Bet *sportium = oBet_nsome(aOBet_get(sportium_row, col));
      if (!pbet && sportium) {
        changed = 1;
        aOBet_set(pbets_row, col, oBet_mk_some(sportium));
      }
    }
  }

  if (changed) bets_write(pbets);
}

void update_run(void) {
  update_results();
  update_points();
  update_bets();
}
