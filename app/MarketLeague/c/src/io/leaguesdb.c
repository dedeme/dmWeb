// Copyright 27-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "io/leaguesdb.h"
#include "dmc/date.h"
#include "dmc/cgi.h"
#include "dmc/match.h"
#include "io/quotes.h"
#include "data/Session.h"
#include "data/Quote.h"
#include "DEFS.h"

static char *mk_leagues_path (void) {
  char *r = path_cat(cgi_home(), "data", "leagues", NULL);
  if (!file_exists(r)) {
    file_mkdir(r);
  }
  return r;
}

static Tp3 *select_first_nicks (time_t date) {
  // Arr<char>
  Arr *nicks1 = arr_new();
  // Arr<char>
  Arr *nicks2 = arr_new();
  // Arr<char>
  Arr *nicks3 = arr_new();
  // Arr<char>
  Arr *nicks = nicks1;
  EACH_IX(quotes_bests(date), char, nk, i) {
    if (i == PLAYERS_PER_LEAGUE) nicks = nicks2;
    if (i == PLAYERS_PER_LEAGUE + PLAYERS_PER_LEAGUE) nicks = nicks3;
    arr_push(nicks, nk);
  }_EACH
  return tp3_new(nicks1, nicks2, nicks3);
}

static Tp3 *select_nicks (char *league_id) {
  char *f = path_cat(mk_leagues_path(), league_id, NULL);
  Session *ss = session_from_js((Js *)file_read(f));

  // Arr<char>
  Arr *nks1 = session_nicks1(ss);
  char *fn1 (MatchStanding *st) {
    return arr_get(nks1, matchStanding_player(st));
  }
  nks1 = arr_map(match_standings(session_l1(ss)), (FCOPY)fn1);
  // Arr<char>
  Arr *nks2 = session_nicks2(ss);
  char *fn2 (MatchStanding *st) {
    return arr_get(nks2, matchStanding_player(st));
  }
  nks2 = arr_map(match_standings(session_l2(ss)), (FCOPY)fn2);
  // Arr<char>
  Arr *nks3 = session_nicks3(ss);
  char *fn3 (MatchStanding *st) {
    return arr_get(nks3, matchStanding_player(st));
  }
  nks3 = arr_map(match_standings(session_l3(ss)), (FCOPY)fn3);

  // Arr<char>
  Arr *nks_old = arr_new();
  arr_cat(nks_old, nks1);
  arr_cat(nks_old, nks2);
  arr_cat(nks_old, nks3);
  time_t date = date_from_str(str_f("%s01", league_id));
  // Arr<char>
  Arr *nks_vol = quotes_vol_filter2(
    date, date_new(1, date_month(date) + 2, date_year(date))
  );

  int ff1 (char *nick) {
    int fn (char *n) { return str_eq(n, nick); }
    return arr_index(nks_vol, (FPRED)fn) != -1;
  }
  arr_filter_in(nks_old, (FPRED)ff1);

  EACH(nks_vol, char, n) {
    int fn (char *nick) { return str_eq(n, nick); }
    if (arr_index(nks_old, (FPRED)fn) == -1)
      arr_push(nks_old, n);
  }_EACH

  nks_vol = arr_new();
  EACH_IX(nks_old, char, nk, ix) {
    if (ix == PLAYERS_PER_LEAGUE) {
      nks1 = nks_vol;
      nks_vol = arr_new();
    } else if (ix == PLAYERS_PER_LEAGUE + PLAYERS_PER_LEAGUE) {
      nks2 = nks_vol;
      nks_vol = arr_new();
    }
    arr_push(nks_vol, nk);
  }_EACH

  return tp3_new(nks1, nks2, nks_vol);
}

// nicksX are Arr<char>
static void make_historic_league(
  Arr *nicks1, Arr *nicks2, Arr *nicks3, char *id
) {
  time_t date = date_from_str(str_cat(id, "01", NULL));

  // nicks is Arr<char>
  MatchLeague *mk (Arr *nicks) {
    time_t d = date;
    arr_shuffle(nicks);
    MatchLeague *l = match_league(1, PLAYERS_PER_LEAGUE);
    while (match_next_round_ix(l) < MATCHES_PER_ROUND) {
      MatchRsRound *rss = opt_nget(
        quotes_results(d, nicks, match_next_round(l))
      );
      if (rss) match_add(l, rss);
      d = date_add(d, 1);
    }
    return l;
  }

  file_write(
    path_cat(mk_leagues_path(), id, NULL),
    (char *)session_to_js(session_new(
      nicks1, mk(nicks1), nicks2, mk(nicks2), nicks3, mk(nicks3)
    ))
  );
}

// nicksX are Arr<char>
static void make_last_league(
  Arr *nicks1, Arr *nicks2, Arr *nicks3, char *id
) {
  time_t date = date_from_str(str_cat(id, "01", NULL));
  time_t last_date = quotes_last_date();

  // nicks is Arr<char>
  MatchLeague *mk (Arr *nicks) {
    time_t d = date;
    arr_shuffle(nicks);
    MatchLeague *l = match_league(1, PLAYERS_PER_LEAGUE);
    while (match_next_round_ix(l) < MATCHES_PER_ROUND && d <= last_date) {
      MatchRsRound *rss = opt_nget(
        quotes_results(d, nicks, match_next_round(l))
      );
      if (rss) match_add(l, rss);
      d = date_add(d, 1);
    }

    if (match_next_round_ix(l) < MATCHES_PER_ROUND && quotes_is_market_open()) {
      MatchRsRound *rss = opt_nget(
        quotes_last_results(nicks, match_next_round(l))
      );
      if (rss) match_add(l, rss);
    }

    return l;
  }

  file_write(
    path_cat(mk_leagues_path(), id, NULL),
    (char *)session_to_js(session_new(
      nicks1, mk(nicks1), nicks2, mk(nicks2), nicks3, mk(nicks3)
    ))
  );
}

// nicks is Arr<char>
static void complete_last_league (time_t date, Arr *nicks, MatchLeague *l) {
  if (match_next_round_ix(l) == MATCHES_PER_ROUND)
    return;

  // Arr<Quote>
  Arr *last_quotes = quotes_last_quotes();
  int has_date (time_t date) {
    EACH(last_quotes, Quote, q) {
      if (quote_date(q) == date) return 1;
    }_EACH
    return 0;
  }

  int round = 0;
  int next_round_ix = match_next_round_ix(l);
  while (round < next_round_ix) {
    if (has_date(date)) ++round;
    date = date_add(date, 1);
  }

  time_t last_date = quotes_last_date();
  while (match_next_round_ix(l) < MATCHES_PER_ROUND && date <= last_date) {
    MatchRsRound *rss = opt_nget(
      quotes_results(date, nicks, match_next_round(l))
    );
    if (rss) match_add(l, rss);
    date = date_add(date, 1);
  }

  if (match_next_round_ix(l) < MATCHES_PER_ROUND && quotes_is_market_open()) {
    MatchRsRound *rss = opt_nget(
      quotes_last_results(nicks, match_next_round(l))
    );
    if (rss) match_add(l, rss);
  }
}

Arr *leaguesdb_sessions (void) {
  // Arr<char>
  Arr *ss = file_dir(mk_leagues_path());

  time_t now = date_now();
  int month = date_month(now);
  time_t last_league_date = month % 2
    ? date_new(1, month, date_year(now))
    : date_new(1, month - 1, date_year(now))
  ;
  char *last_league_id = date_f(last_league_date, "%Y%m");
  int fn (char *l) { return str_eq(l, last_league_id); }
  if (arr_index(ss, (FPRED)fn) == -1) {
    time_t date = date_new(
      1, date_month(last_league_date) - 2, date_year(last_league_date)
    );
    char *id = date_f(date, "%Y%m");
    time_t first_league_date = date_new(1, 1, date_year(now) - 1);
    char *first_league_id = date_f(first_league_date, "%Y%m");
    while (str_greater(id, first_league_id)) {
      int fn (char *l) { return str_eq(l,id); }
      if (arr_index(ss, (FPRED)fn) != -1)
        break;
      date = date_new(1, date_month(date) - 2, date_year(date));
      id = date_f(date, "%Y%m");
    }

    // Tp3<Arr<char>, Arr<char>, Arr<char>>
    Tp3 *t;
    if (str_eq(id, first_league_id))
      t = select_first_nicks(first_league_date);
    else
      t = select_nicks(id);

    // Arr<char>
    Arr *nicks1 = tp3_e1(t);
    // Arr<char>
    Arr *nicks2 = tp3_e2(t);
    // Arr<char>
    Arr *nicks3 = tp3_e2(t);

    while (str_greater(last_league_id, id)) {
      make_historic_league(nicks1, nicks2, nicks3, id);
      t = select_nicks(id);
      nicks1 = tp3_e1(t);
      nicks2 = tp3_e2(t);
      nicks3 = tp3_e2(t);
      date = date_new(1, date_month(date) + 2, date_year(date));
      id = date_f(date, "%Y%m");
    }

    make_last_league(nicks1, nicks2, nicks3, id);
    ss = file_dir(mk_leagues_path());
  }

  arr_sort(ss, (FCMP)str_greater);
  while (arr_size(ss) > 6) {
    file_del(path_cat(mk_leagues_path(), arr_get(ss, 0), NULL));
    arr_remove(ss, 0);
  }

  return ss;
}

// Arr<char>
Arr *leaguesdb_rounds (char *session) {
  time_t now = date_now();
  int month = date_month(now);
  time_t last_league_date = month % 2
    ? date_new(1, month, date_year(now))
    : date_new(1, month - 1, date_year(now))
  ;

  char *f = path_cat(mk_leagues_path(), session, NULL);

  if (str_eq(date_f(last_league_date, "%Y%m"), session)) {
    Session *ss = session_from_js((Js *)file_read(f));
    complete_last_league(last_league_date, session_nicks1(ss), session_l1(ss));
    complete_last_league(last_league_date, session_nicks1(ss), session_l2(ss));
    complete_last_league(last_league_date, session_nicks1(ss), session_l3(ss));

    file_write(f, (char *)session_to_js(ss));
  }

  Session *ss = session_from_js((Js *)file_read(f));
  MatchLeague *l = session_l1(ss);
  // Arr<char>
  Arr *r = arr_new();
  RANGE0(i, match_next_round_ix(l)) {
    arr_push(r, str_right(str_f("0%d", i + 1), -2));
  }_RANGE
  arr_reverse(r);
  return r;
}
