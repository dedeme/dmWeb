// Copyright 27-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "io/quotes.h"
#include "data/Quote.h"
#include "dmc/date.h"
#include "dmc/Darr.h"
#include "DEFS.h"

static double fabs (double n) {
  return n < 0 ? -n : n;
}

static char *nick_model (void) {
  // Arr<Js>
  Arr *nicks_db = js_ra((Js *)file_read(NICKS_DB));
  int id = js_ri(arr_get(nicks_db, 1));
  // Arr<Js>
  Arr *nicks_js = js_ra(arr_get(nicks_db, 2));

  int fn (Js *js) { return js_ri(arr_get(js_ra(js), 0)) == id; }
  return js_rs(arr_get(
    js_ra(opt_oget(
      it_find(it_from(nicks_js), (FPRED)fn),
      arr_get(nicks_js, 0)
    )),
    1
  ));
}

// Returns Arr<char>
static Arr *nicks_list (void) {
  // Arr<Js>
  Arr *nicks_db = js_ra((Js *)file_read(NICKS_DB));
  char *fn (Js *js) { return js_rs(arr_get(js_ra(js), 1)); }
  return arr_map(js_ra(arr_get(nicks_db, 2)), (FCOPY)fn);
}

// Returns Arr<Quote>
static Arr *nick_quotes (char *nk) {
  return quote_table(file_read(
    path_cat(QUOTES_DIR, str_f("%s.db", nk), NULL)
  ));
}

static double historic_last_close (char *nick) {
  return quote_close(arr_get(nick_quotes(nick), 0));
}

static double daily_close (char *nick) {
  // Map<Js>
  Map *cls = js_ro((Js *)file_read(DAILY_QUOTES_DB));
  Js *cl = opt_nget(map_get(cls, nick));
  return cl ? js_rd(cl) : -1;
}

// Returns the increment of date quote relative to its previous one.
// If some quote is -1, returns 1000000
static double delta_quotes (char *nick, time_t date) {
  double r = 1000000;
  EACH(nick_quotes(nick), Quote, q) {
    double close = quote_close(q);
    if (r < 100000) {
      if (close > 0) return (r - close) / close;
      else return 1000000;
    }
    time_t d = quote_date(q);
    if (d == date && close > 0) r = close;
    else if (d <= date) break;
  }_EACH
  return r;
}

static char *multi_marquet_activity (void) {
  // Map<Js>
  Map *cf = js_ro((Js *)file_read(MM_CONF_DB));
  Js *r = opt_nget(map_get(cf, "activity"));
  if (r) return js_rs(r);
  else return "Sleeping (2)";
}

// Returns the increment of date quote relative to its previous one.
// If some quote is -1, returns 1000000
static double delta_last_quotes (char *nick) {
  double hclose = historic_last_close(nick);
  double dclose = daily_close(nick);

  if (hclose == -1 || dclose == -1) return 1000000;
  else return (dclose - hclose) / hclose;
}

Arr *quotes_bests (time_t date) {
  time_t vol_date = date_new(1, date_month(date) - 2, date_year(date));
  //<Tp3<char, int, double>> (nick, vol, pond)
  Tp3 *fn (char *nick) { return tp3_new(nick, MALLOC(int), MALLOC(double)); }
  // Arr<Tp3<char, int, double>> (nick, vol, pond)
  Arr *nicks = arr_map(nicks_list(), (FCOPY)fn);

  // Arr<Tp3<char, int, double>> (nick, vol, pond)
  Arr *nicks2 = arr_new();
  EACH(nicks, Tp3, t) {
    int vol = 0;
    int voln = 0;
    double after = -1;
    double before = -1;
    EACH(nick_quotes(tp3_e1(t)), Quote, q) {
      time_t d = quote_date(q);
      before = quote_close(q);
      if (after < 0 && d < date)
        after = before;
      if (d < date && d >= vol_date) {
        vol += quote_vol(q);
        ++voln;
      }
    }_EACH
    if (before > 0) {
      *((int *)tp3_e2(t)) = vol / voln;
      *((double *)tp3_e3(t)) = (after - before) / before;
      arr_push(nicks2, t);
    }
  }_EACH

  // 't1' adn 't2' are <Tp3<char, int, double>> (nick, vol, pond)
  int fgreater1 (Tp3 *t1, Tp3 *t2) {
    return *((int *) tp3_e2(t1)) < *((int *)tp3_e2(t2));
  }
  // 't1' adn 't2' are <Tp3<char, int, double>> (nick, vol, pond)
  int fgreater2 (Tp3 *t1, Tp3 *t2) {
    return *((double *)tp3_e3(t1)) < *((double *)tp3_e3(t2));
  }
  // 't' is //<Tp3<char, int, double>> (nick, vol, pond)
  char *fmap (Tp3 *t) {
    //printf("%s:%d:%.4f\n",
    //  (char*)tp3_e1(t), *(int*)tp3_e2(t), *(double*)tp3_e3(t));
    return tp3_e1(t);
  }
  return it_to(it_map(
    it_sort(
      it_take(
        it_sort(it_from(nicks2), (FCMP)fgreater1),
        PLAYERS_PER_LEAGUE * 3
      ),
      (FCMP)fgreater2
    ),
    (FCOPY)fmap
  ));
}

Opt *quotes_results(time_t date, Arr *nicks, MatchRound *round) {
  int fn (Quote *q) { return quote_date(q) == date; }
  if (!it_contains(it_from(nick_quotes(nick_model())), (FPRED)fn))
    return opt_empty();

  double sum = 0;
  int n = 0;
  Darr *points = darr_new();
  EACH(matchRound_matches(round), MatchPlayers, m) {
    double dif = 0;
    double dup = delta_quotes(arr_get(nicks, matchPlayers_up(m)), date);
    double ddown = delta_quotes(arr_get(nicks, matchPlayers_down(m)), date);
    if (dup < 100000 && ddown < 100000)
      dif = dup - ddown;

    sum += fabs(dif);
    ++n;
    darr_push(points, dif);
  }_EACH
  double cut = sum / (n + n);

  MatchRsRound *rs = matchRsRound_new();
  // Arr<MatchResult>
  Arr *ars = matchRsRound_matches(rs);
  DEACH(points, p) {
    arr_push(ars, matchResult_new(
      fabs(p) <= cut ? MATCH_RESULT_DRAW
        : p > 0 ? MATCH_RESULT_UP : MATCH_RESULT_DOWN,
      p
    ));
  }_EACH

  return opt_new(rs);
}

Opt *quotes_last_results(Arr *nicks, MatchRound *round) {
  if (str_eq(multi_marquet_activity(), "Sleeping (2)"))
    return opt_empty();

  double sum = 0;
  int n = 0;
  Darr *points = darr_new();
  EACH(matchRound_matches(round), MatchPlayers, m) {
    double dif = 0;
    double dup = delta_last_quotes(arr_get(nicks, matchPlayers_up(m)));
    double ddown = delta_last_quotes(arr_get(nicks, matchPlayers_down(m)));
    if (dup < 100000 && ddown < 100000)
      dif = dup - ddown;

    sum += fabs(dif);
    ++n;
    darr_push(points, dif);
  }_EACH
  double cut = sum / (n + n);

  MatchRsRound *rs = matchRsRound_new();
  // Arr<MatchResult>
  Arr *ars = matchRsRound_matches(rs);
  DEACH(points, p) {
    arr_push(ars, matchResult_new(
      fabs(p) <= cut ? MATCH_RESULT_DRAW
        : p > 0 ? MATCH_RESULT_UP : MATCH_RESULT_DOWN,
      p
    ));
  }_EACH

  return opt_new(rs);
}

Arr *quotes_vol_filter (int month, int year) {
  time_t end = date_new(1, month, year);
  time_t start = date_new(1, month - 2, year);
  return quotes_vol_filter2(start, end);
}

Arr *quotes_vol_filter2 (time_t start, time_t end) {
  //<Tp<char, int> (nick, vol)
  Tp *fn (char *nick) { return tp_new(nick, MALLOC(int)); }
  // Arr<Tp<char, int, double>> (nick, vol, pond)
  Arr *nicks = arr_map(nicks_list(), (FCOPY)fn);

  // Arr<Tp<char, int> (nick, vol)
  Arr *nicks2 = arr_new();
  EACH(nicks, Tp, t) {
    int vol = 0;
    int voln = 0;
    EACH(nick_quotes(tp_e1(t)), Quote, q) {
      time_t d = quote_date(q);
      if (d < end && d >= start) {
        vol += quote_vol(q);
        ++voln;
      }
    }_EACH
    if (voln > 0) {
      *((int *)tp_e2(t)) = vol / voln;
      arr_push(nicks2, t);
    }
  }_EACH

  // 't1' adn 't2' are <Tp<char, int>> (nick, vol)
  int fgreater1 (Tp *t1, Tp *t2) {
    return *((int *) tp_e2(t1)) < *((int *)tp_e2(t2));
  }
  // 't' is //<Tp<char, int>> (nick, vol)
  char *fmap (Tp *t) {
    //printf("%s:%d\n",
    //  (char*)tp_e1(t), *(int*)tp_e2(t));
    return tp_e1(t);
  }
  return it_to(it_map(
    it_take(
      it_sort(it_from(nicks2), (FCMP)fgreater1),
      PLAYERS_PER_LEAGUE * 3
    ),
    (FCOPY)fmap
  ));
}

time_t quotes_last_date (void) {
  return quote_date(arr_get(nick_quotes(nick_model()), 0));
}

Arr *quotes_last_quotes (void) {
  return arr_take(nick_quotes(nick_model()), MATCHES_PER_ROUND * 2);
}

int quotes_is_market_open (void) {
  time_t now = date_now();
  char *nows = date_to_str(now);
  char *weekd = date_f(now, "%w");
  if (*weekd == '0' || *weekd == '1')
    return 0;

  // Map<Js>
  Map *cal = js_ro((Js *)file_read(CALENDAR_DB));

  int fn (char *d) { return str_eq(d, nows); }
  if (it_contains(
    it_from(arr_from_js(opt_get(map_get(cal, "holidays")), (FFROM)js_rs)),
    (FPRED)fn
  )) {
    return 0;
  }

  int h = atoi(date_f(now, "%H"));
  int m = atoi(date_f(now, "%M"));

  int in_time (int sh, int sm, int eh, int em) {
    return h < sh ? 0
      : h == sh && m < sm ? 0
        : h < eh ? 1
          : h == eh && m < em ? 1
            : 0
    ;
  }

  int fn2 (Js *d) { return str_eq(js_rs(arr_get(js_ra(d), 0)), nows); }
  Js *sp = opt_nget(it_find(it_from(
    js_ra(opt_get(map_get(cal, "specialDays")))
  ), (FPRED)fn2));
  if (sp) {
    // Arr<Js>
    Arr *a = js_ra(sp);
    int sh = js_ri(arr_get(a, 1));
    int sm = js_ri(arr_get(a, 2));
    int eh = js_ri(arr_get(a, 3));
    int em = js_ri(arr_get(a, 4));
    return in_time(sh, sm, eh, em);
  }

  // Arr<js>
  Arr *a = js_ra(opt_get(map_get(cal, "general")));
  int sh = js_ri(arr_get(a, 0));
  int sm = js_ri(arr_get(a, 1));
  int eh = js_ri(arr_get(a, 2));
  int em = js_ri(arr_get(a, 3));
  return in_time(sh, sm, eh, em);
}
