// Copyright 25-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "net/sportium.h"
#include <stdlib.h>
#include "dmc/ext.h"
#include "dmc/Tx.h"
#include "dmc/err.h"
#include "dmc/str.h"
#include "dmc/date.h"
#include "data/cts.h"
#include "data/Tbet/ATbet.h"

#include <stdio.h>

ATbet *read_bets(Tx *t) {
  ATbet *r = aTbet_new();

  int ix = tx_index(t, "<h4 class=\"expander-button\">");
  for (;;) {
    if (ix == -1) break;
    t = tx_right(t, ix + 1);

    ix = tx_index(tx_left(t, 100), "La Liga");
    if (ix != -1) break;

    ix = tx_index(t, "<h4 class=\"expander-button\">");
  }

  if (ix == -1) return r;
  t = tx_right(t, ix);

  ix = tx_index(t, "<h4 class=\"expander-button\">");
  if (ix == -1) ix = tx_length(t);
  t = tx_left(t, ix);

  for (;;) {
    int ix = tx_index(t, "time coupon-scoreboard");
    if (ix == -1) break;

    t = tx_right(t, ix);
    ix = tx_index(t, "date");
    if (ix == -1)
      FAIL(str_f("'date' not found in\n%s", str_left(tx_to(t), 80)));
    t = tx_right(t, ix);
    ix = tx_cindex(t, '>');
    if (ix == -1)
      FAIL(str_f("'>' not found in\n%s", str_left(tx_to(t), 80)));
    t = tx_right(t, ix + 1);
    ix = tx_cindex(t, ' ');
    if (ix == -1)
      FAIL(str_f("' ' not found in\n%s", str_left(tx_to(t), 80)));

    int day = atoi(tx_to(tx_left(t, ix)));

    ix = tx_index(t, "\"seln-name\"");
    if (ix == -1)
      FAIL(str_f("'seln-name' not found in\n%s", str_left(tx_to(t), 80)));
    t = tx_right(t, ix);
    ix = tx_cindex(t, '>');
    if (ix == -1)
      FAIL(str_f("'>' not found in\n%s", str_left(tx_to(t), 80)));
    t = tx_right(t, ix + 1);
    ix = tx_cindex(t, '<');
    if (ix == -1)
      FAIL(str_f("'<' not found in\n%s", str_left(tx_to(t), 80)));

    char *home = str_trim(tx_to(tx_left(t, ix)));

    ix = tx_index(t, "\"price dec\"");
    if (ix == -1)
      FAIL(str_f("'price dec' not found in\n%s", str_left(tx_to(t), 80)));
    t = tx_right(t, ix);
    ix = tx_cindex(t, '>');
    if (ix == -1)
      FAIL(str_f("'>' not found in\n%s", str_left(tx_to(t), 80)));
    t = tx_right(t, ix + 1);
    ix = tx_cindex(t, '<');
    if (ix == -1)
      FAIL(str_f("'<' not found in\n%s", str_left(tx_to(t), 80)));

    double r1 = atof(str_trim(tx_to(tx_left(t, ix))));

    ix = tx_index(t, "\"price dec\"");
    if (ix == -1)
      FAIL(str_f("'price dec' not found in\n%s", str_left(tx_to(t), 80)));
    t = tx_right(t, ix);
    ix = tx_cindex(t, '>');
    if (ix == -1)
      FAIL(str_f("'>' not found in\n%s", str_left(tx_to(t), 80)));
    t = tx_right(t, ix + 1);
    ix = tx_cindex(t, '<');
    if (ix == -1)
      FAIL(str_f("'<' not found in\n%s", str_left(tx_to(t), 80)));

    double rx = atof(str_trim(tx_to(tx_left(t, ix))));

    ix = tx_index(t, "\"seln-name\"");
    if (ix == -1)
      FAIL(str_f("'seln-name' not found in\n%s", str_left(tx_to(t), 80)));
    t = tx_right(t, ix);
    ix = tx_cindex(t, '>');
    if (ix == -1)
      FAIL(str_f("'>' not found in\n%s", str_left(tx_to(t), 80)));
    t = tx_right(t, ix + 1);
    ix = tx_cindex(t, '<');
    if (ix == -1)
      FAIL(str_f("'<' not found in\n%s", str_left(tx_to(t), 80)));

    char *out = str_trim(tx_to(tx_left(t, ix)));

    ix = tx_index(t, "\"price dec\"");
    if (ix == -1)
      FAIL(str_f("'price dec' not found in\n%s", str_left(tx_to(t), 80)));
    t = tx_right(t, ix);
    ix = tx_cindex(t, '>');
    if (ix == -1)
      FAIL(str_f("'>' not found in\n%s", str_left(tx_to(t), 80)));
    t = tx_right(t, ix + 1);
    ix = tx_cindex(t, '<');
    if (ix == -1)
      FAIL(str_f("'<' not found in\n%s", str_left(tx_to(t), 80)));

    double r2 = atof(str_trim(tx_to(tx_left(t, ix))));

    aTbet_push(r, tbet_new(day, home, out, bet_new(r1, rx, r2)));
  }

  return r;
}

AAOBet *sportium_read (void) {
  char *html = ext_wget(
    "https://sports.sportium.es/es"
      "?utm_campaign=territorio-liga-home&utm_medium=display&utm_source=laliga",
    0
  );

  Tx *t = tx_new(html);
  ATbet *bets = read_bets(t);

  AAOBet *r = aAOBet_new_nones();

  int today = date_day(date_now());

  Achar *teams = cts_sportium_teams();
  Tbet **p = bets->es;
  while (p < bets->end) {
    Tbet *b = *p++;

    if (b->day == today) {
      /**/int home_index(char *e) { return str_eq(e, b->home); }
      int home_team = achar_index(teams, home_index);
      if (home_team == -1)
        FAIL(str_f("'%s' team not found", b->home));

      /**/int out_index(char *e) { return str_eq(e, b->out); }
      int out_team = achar_index(teams, out_index);
      if (out_team == -1)
        FAIL(str_f("'%s' team not found", b->out));
      aOBet_set(
        aAOBet_get(r, home_team),
        out_team,
        oBet_mk_some(b->bet)
      );
    }
  }

  return r;
}
