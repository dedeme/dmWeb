// Copyright 17-Jul-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "io.h"
#include <math.h>
#include "dmc/cgi.h"
#include "Match.h"
#include "Mark.h"
#include "DEFS.h"

static char *conf_path () {
  return str_cat(cgi_home(), CONF_FILE, NULL);
}

static void mkconf (void) {
  char *cf = conf_path();
  file_mkdir(path_parent(cf));
  file_write(cf, "{}");
}

// returns Map[Js]
static Map *rconf () {
  char *f = conf_path();
  if (!file_exists(f)) mkconf();

  return js_ro((Js *)file_read(f));
}

static void cf_set (char *key, Js *value) {
  // Map[Js]
  Map *db = rconf();
  map_put(db, key, value);
  file_write(conf_path(), (char *)js_wo(db));
}

// ---------------------------------------------------------------------------//

DataAll *io_init () {
  // Arr[char]
  Arr *nicks = io_nicks();

  // Arr[Arr[char]]
  Arr *fnks (enum LeagueGroup group) {
    double dif (Darr *ds) {
      double first = -1;
      DEACH(ds, d)
        if (d > 0) {
          first = d;
          break;
        }
      _EACH
      if (first < 0) return 0;
      double last = first;
      for (int i = darr_size(ds) - 1; i >= 0; --i) {
        if (darr_get(ds, i) > 0) {
          last = darr_get(ds, i);
          break;
        }
      }
      return (last - first) / first;
    }

    // Arr[Tp[char, *double]]
    Arr *nk_ponds = arr_new();
    EACH(nicks, char, nick)
      double *d = ATOMIC(sizeof(double));
      *d = dif(io_quotes(group, nick));
      arr_push(nk_ponds, tp_new(nick, d));
    _EACH

    // Reverse order
    int fn (Tp *n_p1, Tp *n_p2) {
      return *((double *)tp_e2(n_p1)) < *((double *)tp_e2(n_p2));
    }
    arr_sort(nk_ponds, (FCMP)fn);

    void *fmap (Tp *n_p) { return tp_e1(n_p); }
    // Arr[char]
    Arr *nks = it_to(it_map(it_from(nk_ponds), (FCOPY)fmap));

    int nks_size = arr_size(nks);
    int gr1n = nks_size / 3;
    int gr2n = gr1n + gr1n;
    // Arr[Arr[char]]
    Arr *r = arr_new();
    arr_push(r, it_to(it_take(it_from(nks), gr1n)));
    arr_push(r, it_to(
      it_drop(it_take(it_from(nks), gr2n), gr1n)
    ));
    arr_push(r, it_to(it_drop(it_from(nks), gr2n)));

    return r;
  }

  // Arr[Arr[char]]
  Arr *d_nks = fnks(DAILY_G);
  // Arr[Arr[char]]
  Arr *s_nks = fnks(SHORT_G);
  // Arr[Arr[char]]
  Arr *m_nks = fnks(MEDIUM_G);
  // Arr[Arr[char]]
  Arr *l_nks = fnks(LONG_G);

  char *date = io_last_historic_date();
  DataAll *r = dataAll_new(
    dataPrevious_new(
      dataPreviousGroup_new(
        arr_get(d_nks, 0), arr_get(d_nks, 1), arr_get(d_nks, 2)
      ),
      dataPreviousGroup_new(
        arr_get(s_nks, 0), arr_get(s_nks, 1), arr_get(s_nks, 2)
      ),
      dataPreviousGroup_new(
        arr_get(m_nks, 0), arr_get(m_nks, 1), arr_get(m_nks, 2)
      ),
      dataPreviousGroup_new(
        arr_get(l_nks, 0), arr_get(l_nks, 1), arr_get(l_nks, 2)
      )
    ),
    dataCurrent_new(
      dataLeagueGroup_new(
        io_count_daily_quotes(),
        io_mk_league(arr_get(d_nks, 0), DAILY_G),
        io_mk_league(arr_get(d_nks, 1), DAILY_G),
        io_mk_league(arr_get(d_nks, 2), DAILY_G)
      ),
      dataLeagueGroup_new(
        date,
        io_mk_league(arr_get(s_nks, 0), SHORT_G),
        io_mk_league(arr_get(s_nks, 1), SHORT_G),
        io_mk_league(arr_get(s_nks, 2), SHORT_G)
      ),
      dataLeagueGroup_new(
        date,
        io_mk_league(arr_get(m_nks, 0), MEDIUM_G),
        io_mk_league(arr_get(m_nks, 1), MEDIUM_G),
        io_mk_league(arr_get(m_nks, 2), MEDIUM_G)
      ),
      dataLeagueGroup_new(
        date,
        io_mk_league(arr_get(l_nks, 0), LONG_G),
        io_mk_league(arr_get(l_nks, 1), LONG_G),
        io_mk_league(arr_get(l_nks, 2), LONG_G)
      )
    )
  );

  return r;
}

void io_set_lang (char *lang) {
  cf_set("lang", js_ws(lang));
}

char *io_lang () {
  return js_rs(opt_oget(map_get(rconf(), "lang"), js_ws("es")));
}

// Arr[char]
Arr *io_nicks () {
  // Arr[Js]
  Arr *nicks_db = js_ra((Js *)file_read(NICKS_FILE));
  // Arr[Js]
  Arr *nicks = js_ra(arr_get(nicks_db, 2));
  // Arr[char]
  Arr *r = arr_new();
  EACH(nicks, Js, js)
    // Arr[char]
    Arr *fields = js_ra(js);
    if (js_rb(arr_get(fields, 2))) {
      arr_push(r, js_rs(arr_get(fields, 1)));
    }
  _EACH

  return r;
}

Darr *io_quotes (enum LeagueGroup group,  char *nick) {
  if (group == DAILY_G) {
    // Arr[Js]
    Arr *qs_db = js_ra(
      (Js *)file_read(str_f("%s/%s", DAILY_QUOTES_DIR, nick))
    );
    // Arr[Js]
    Arr *qs = js_ra(arr_get(qs_db, 1));
    Darr *r = darr_new();
    EACH(qs, Js, js)
      // Arr[Js]
      Arr *fields = js_ra(js);
      darr_push(r, js_rd(arr_get(fields, 1)));
    _EACH

    return r;
  }

  char *qs = file_read(str_f("%s/%s.db", HISTORIC_QUOTES_DIR, nick));
  int n = group == SHORT_G ? SHORT_NQUOTES
    : group == MEDIUM_G ? MEDIUM_NQUOTES
      : LONG_NQUOTES;
  Darr *r = darr_new();
  void fn (char *line) {
    darr_push(r, js_rd(arr_get(str_csplit(line, ':'), 2)));
  }
  it_each(
    it_take(it_from(str_csplit(qs, '\n')), n),
    (FPROC)fn
  );
  darr_reverse(r);

  return r;
}

char *io_last_historic_date () {
  return str_new(
    arr_get(
      str_csplit(
        arr_get(
          str_csplit(
            file_read(str_f(
              "%s/%s",
              HISTORIC_QUOTES_DIR,
              arr_get(file_dir(HISTORIC_QUOTES_DIR), 0)
            )),
            '\n'
          ), 0
        ), ':'
      ),
      0
    )
  );
}

char *io_count_daily_quotes () {
  return str_f(
    "%d",
    darr_size(
      io_quotes(
        DAILY_G,
        arr_get(file_dir(DAILY_QUOTES_DIR), 0)
      )
    )
  );
}

DataLeague *io_mk_league (Arr *nicks, enum LeagueGroup group) {
  double first_valid (Darr *qs, int ix) {
    for (int i = ix; i < darr_size(qs); ++i) {
      if (darr_get(qs, i) > 0) return darr_get(qs, i);
    }
    return -1;
  }

  double last_valid (Darr *qs) {
    for (int i = darr_size(qs) - 1; i >= 0; --i) {
      if (darr_get(qs, i) > 0) return darr_get(qs, i);
    }
    return -1;
  }

  int nsize = arr_size(nicks);
  int odd = nsize % 2;
  int nrounds = nsize - 1 + odd;
  // Arr[Darr]
  Arr *scores = arr_new();
  EACH(nicks, char, nk)
    Darr *qs = io_quotes(group, nk);
    int qsize = darr_size(qs);
    Darr *sc = darr_new();
    double previous = first_valid(qs, 0);
    for (int i = 1; i < nrounds; ++i) {
      if (previous < 0) break;
      double next = first_valid(qs, qsize * i / nrounds);
      darr_push(sc, (next - previous) / previous);
      previous = next;
    }

    if (previous < 0) {
      darr_clear(sc);
      REPEAT(nrounds)
        darr_push(sc, -1);
      _REPEAT
    } else {
      darr_push(sc, (last_valid(qs) - previous) / previous);
    }

    arr_push(scores, sc);

  _EACH

  // Arr[Arr[Match]]
  Arr *rounds = match_rounds(nsize + odd);
  if (arr_size(rounds) != nrounds)
    EXC_ILLEGAL_STATE(str_f(
      "'arr_size(rounds)' (%d) != nrounds (%d)", arr_size(rounds), nrounds
    ))

  // Arr[Darr]
  Arr *drounds = arr_new();
  int nrs = 0;
  double sumrs = 0;
  EACH_IX(rounds, Arr, a, iround)
    Darr *drss = darr_new();
    EACH(a, Match, m)
      double rs;
      if (odd && (match_up(m) == nsize || match_down(m) == nsize)) {
        rs = -2000;
      } else {
        rs = darr_get(arr_get(scores, match_up(m)), iround) -
             darr_get(arr_get(scores, match_down(m)), iround);
      }
      darr_push(drss, rs);
      ++nrs;
      sumrs += fabs(rs);
    _EACH
    arr_push(drounds, drss);
  _EACH

  double draw_cut = sumrs / (nrs + nrs);

  // Arr[Arr[DataRoundResult]]
  Arr *results = arr_new();
  EACH(drounds, Darr, rd)
    /// Arr[DataMatchResults]
    Arr *matches = arr_new();
    DEACH(rd, rs)
      if (rs < -1000)
        arr_push(matches, dataMatchResult_new(rs, -1));
      else if (fabs(rs) < draw_cut)
        arr_push(matches, dataMatchResult_new(rs, 0));
      else if (rs > 0)
        arr_push(matches, dataMatchResult_new(rs, 1));

      else
        arr_push(matches, dataMatchResult_new(rs, 2));
    _EACH
    arr_push(results, dataRoundResult_new(matches));
  _EACH

  return dataLeague_new(nicks, results);
}

// Tp[DataPreviousGroup, DataLeagueGroup]
static Tp *gupdate (
  enum LeagueGroup group,
  DataPreviousGroup *previous, DataLeagueGroup *league
) {
  char *code = group == DAILY_G
    ? io_count_daily_quotes()
    : io_last_historic_date()
  ;

  int update = atoi(dataLeagueGroup_code(league)) != atoi(code);

  if (!update) {
    return tp_new(previous, league);
  }

  if (group == DAILY_G && atoi(dataLeagueGroup_code(league)) < atoi(code)) {
    // Arr[char]
    Arr *fnicks = dataPreviousGroup_first(previous);
    // Arr[char]
    Arr *snicks = dataPreviousGroup_second(previous);
    // Arr[char]
    Arr *tnicks = dataPreviousGroup_third(previous);
    return tp_new(
      previous,
      dataLeagueGroup_new(
        code,
        io_mk_league(fnicks, group),
        io_mk_league(snicks, group),
        io_mk_league(tnicks, group)
      )
    );
  }

  // Arr[char]
  Arr *fnicks0 = mark_ranking(dataLeagueGroup_first(league));
  // Arr[char]
  Arr *snicks0 = mark_ranking(dataLeagueGroup_second(league));
  // Arr[char]
  Arr *tnicks0 = mark_ranking(dataLeagueGroup_third(league));

  // Arr[char]
  Arr *fnicks = arr_new();
  // Arr[char]
  Arr *snicks = arr_new();
  // Arr[char]
  Arr *tnicks = arr_new();

  int i;
  for (i = 0; i < arr_size(fnicks0) - 3; ++i)
    arr_push(fnicks, arr_get(fnicks0, i));
  for (; i < arr_size(fnicks0); ++i)
    arr_push(snicks, arr_get(fnicks0, i));

  for (i = 0; i < 3; ++i)
    arr_push(fnicks, arr_get(snicks0, i));
  for (; i < arr_size(snicks0) - 3; ++i)
    arr_push(snicks, arr_get(snicks0, i));
  for (; i < arr_size(snicks0); ++i)
    arr_push(tnicks, arr_get(snicks0, i));

  for (i = 0; i < 3; ++i)
    arr_push(snicks, arr_get(tnicks0, i));
  for (; i < arr_size(tnicks0); ++i)
    arr_push(tnicks, arr_get(tnicks0, i));

  return tp_new(
    dataPreviousGroup_new(fnicks, snicks, tnicks),
    dataLeagueGroup_new(
      code,
      io_mk_league(fnicks, group),
      io_mk_league(snicks, group),
      io_mk_league(tnicks, group)
    )
  );
}

DataAll *io_update (DataAll *data) {
  // Tp[DataPreviousGroup, DataLeagueGroup]
  Tp *daily_g = gupdate(
    DAILY_G,
    dataPrevious_daily_g(dataAll_previous(data)),
    dataCurrent_daily_g(dataAll_current(data))
  );
  // Tp[DataPreviousGroup, DataLeagueGroup]
  Tp *short_g = gupdate(
    SHORT_G,
    dataPrevious_short_g(dataAll_previous(data)),
    dataCurrent_short_g(dataAll_current(data))
  );
  // Tp[DataPreviousGroup, DataLeagueGroup]
  Tp *medium_g = gupdate(
    MEDIUM_G,
    dataPrevious_medium_g(dataAll_previous(data)),
    dataCurrent_medium_g(dataAll_current(data))
  );
  // Tp[DataPreviousGroup, DataLeagueGroup]
  Tp *long_g = gupdate(
    LONG_G,
    dataPrevious_long_g(dataAll_previous(data)),
    dataCurrent_long_g(dataAll_current(data))
  );

  return dataAll_new(
    dataPrevious_new(
      tp_e1(daily_g), tp_e1(short_g), tp_e1(medium_g), tp_e1(long_g)
    ),
    dataCurrent_new(
      tp_e2(daily_g), tp_e2(short_g), tp_e2(medium_g), tp_e2(long_g)
    )
  );
}
