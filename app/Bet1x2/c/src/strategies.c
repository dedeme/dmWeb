// Copyright 28-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "strategies.h"
#include "dmc/err.h"
#include "dmc/str.h"
#include "strategies/Ever1.h"
#include "strategies/Everx.h"
#include "strategies/Ever2.h"
#include "strategies/BestIncomes.h"
#include "strategies/MediumIncomes.h"
#include "strategies/WorstIncomes.h"
#include "strategies/BestTeam.h"
#include "strategies/WorstTeam.h"

AStrategy *strategies_list(void) {
  return aStrategy_new_from(
    ever1_mk(),
    everx_mk(),
    ever2_mk(),
    bestIncomes_mk(),
    mediumIncomes_mk(),
    worstIncomes_mk(),
    bestTeam_mk(),
    worstTeam_mk(),
    NULL
  );
}

Strategy *strategies_get (char *id) {
  AStrategy *a = strategies_list();
  Strategy **p = a->es;
  while (p < a->end) {
    Strategy *s = *p++;
    if (str_eq(s->id(), id)) return s;
  }
  return FAIL(str_f("Strategy '%s' not found.", id));
}

Profits *strategies_year_profits (
  Strategy *s,
  char *description,
  AAOResult *results,
  AAOResult *points,
  AAOBet *bets
) {
  int size = aAOResult_size(results);
  int hits = 0;
  int fails = 0;
  double amount = 0;

  for (int i = 0; i < size; ++i) {
    for (int j = 0; j < size; ++j) {
      Result *r= oResult_nsome(aOResult_get(aAOResult_get(results, i), j));
      Result *p= oResult_nsome(aOResult_get(aAOResult_get(points, i), j));
      Bet *b= oBet_nsome(aOBet_get(aAOBet_get(bets, i), j));
      if (r && p && b) {
        double pfs = s->profits(r, p, b);
        if (pfs < 0) ++fails; else ++hits;
        amount += pfs;
      }
    }
  }

  return profits_new(description, hits, fails, amount);
}

AProfits *strategies_years_profits (
  Strategy *s,
  Achar *years,
  AAAOResult *results,
  AAAOResult *points,
  AAAOBet *bets
) {
  AProfits *r = aProfits_new();

  char **yearsp = years->es;
  AAOResult **resultsp = results->es;
  AAOResult **pointsp = points->es;
  AAOBet **betsp = bets->es;
  while (yearsp < years->end) {
    char *y = *yearsp++;
    AAOResult *rs = *resultsp++;
    AAOResult *ps = *pointsp++;
    AAOBet *bs = *betsp++;

    aProfits_push(r, strategies_year_profits(s, y, rs, ps, bs));
  }

  return r;
}

AProfits *strategies_year_group_profits (
  AStrategy *ss,
  AAOResult *results,
  AAOResult *points,
  AAOBet *bets
) {
  AProfits *r = aProfits_new();

  Strategy **p = ss->es;
  while (p < ss->end) {
    Strategy *s = *p++;
    aProfits_push(
      r, strategies_year_profits(s, s->id(), results, points, bets)
    );
  }

  return r;
}

AProfits *strategies_years_group_profits (
  AStrategy *ss,
  AAAOResult *results,
  AAAOResult *points,
  AAAOBet *bets
) {
  AProfits *r = aProfits_new();

  Strategy **sp = ss->es;
  while (sp < ss->end) {
    AProfits *sprf = aProfits_new();

    Strategy *s = *sp++;
    AAOResult **resultsp = results->es;
    AAOResult **pointsp = points->es;
    AAOBet **betsp = bets->es;

    while (resultsp < results->end) {
      AAOResult *rs = *resultsp++;
      AAOResult *ps = *pointsp++;
      AAOBet *bs = *betsp++;

      aProfits_push(sprf, strategies_year_profits(s, "", rs, ps, bs));
    }

    aProfits_push(r, aProfits_sum(sprf, s->id()));
  }

  return r;
}

