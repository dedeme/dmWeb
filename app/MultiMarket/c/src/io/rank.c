// Copyright 28-Nov-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "io/rank.h"
#include "dmc/date.h"
#include "DEFS.h"
#include "io/io.h"
#include "io/quotes.h"
#include "io/fleasdb.h"
#include "data/dfleas/dfleas__models.h"

static char *drank = NULL;
static char *pooldb = NULL;
static char *rankdb = NULL;

// Pool ------------------------------------------------------------------------

// Arr<RankEntry>
static Arr *read_pool (void) {
  if (!pooldb) EXC_ILLEGAL_STATE("'pool.db' was not intiliazed")

  Js *js = (Js *)file_read(pooldb);
  return arr_from_js(js, (FFROM)rankEntry_from_js);
}

// pool is Arr<RankEntry>
static void save_pool (Arr *pool) {
  if (!pooldb) EXC_ILLEGAL_STATE("'pool.db' was not intiliazed")

  char *s = (char *)arr_to_js(pool, (FTO)rankEntry_to_js);
  file_write(pooldb, s);
}

static void update_pool (void) {
  int min_sells = HISTORIC_QUOTES / MIN_SELLS;
  int max_sells = HISTORIC_QUOTES / MAX_SELLS;
  double max_sells_sum = (double)max_sells / 2;
  double max_sells_mul =
      (double)max_sells / (INITIAL_CAPITAL + INITIAL_CAPITAL);

  // Arr<RankEntry>
  Arr *pool = read_pool();

  void add (int ix) {
    EACH(dfleas__models(), Model, md) {
      char *mdname = model_name(md);
      // Arr<RsBests>
      Arr *bests = fleasdb_bests_read(mdname);
      if (arr_size(bests) > ix) {
        RsBests *rsB = arr_get(bests, ix);
        Rs *rs = rsWeb_result(rsBests_result(rsB));
        Flea *f = rs_flea(rs);
        int fn (RankEntry *e) {
          return str_eq(flea_name(f), flea_name(rankEntry_flea(e))) &&
            str_eq(mdname, rankEntry_model_name(e))
          ;
        }

        RsAssets *rsA = rs_assets(rs);
        int sells = rsAssets_sells(rsA);
        int okSells = sells >= min_sells &&
          sells <= max_sells_sum + rsAssets_assets(rsA) * max_sells_mul;

        if (!arr_any(pool, (FPRED)fn) && okSells)
          arr_push(pool, rankEntry_new(mdname, f, 0, 0));
      }
    }_EACH
  }

  int c = 0;
  add(c);
  while (arr_size(pool) < RANKING_POOL_NUMBER && c < RANKING_POOL_NUMBER)
    add(++c);

  save_pool(pool);
}

// Return Arr<RankEvalEntry> ascendingly sorted , pool is Arr<RankEntry>
// RankEntries without existent model are removed.
static Arr *evaluate (Arr *pool) {
  time_t now = date_now();
  char *last_date = opt_nget(quotes_last_date());
  if (last_date) now = date_from_str(last_date);

  Qmatrix *opens = opt_nget(quotes_opens());
  Qmatrix *closes = opt_nget(quotes_closes());

  // Arr<RankEvalEntry>
  Arr *pool_ev = arr_new();

  double assets_mx = -1000000;
  double assets_mn = 1000000;
  double profits_mx = -1000000;
  double profits_mn = 1000000;
  double days_mx = 0;

  EACH(pool, RankEntry, e) {
    Flea *f = rankEntry_flea(e);
    char *mdname = rankEntry_model_name(e);
    Model *model = opt_nget(rankEntry_model(e));
    if (model) {
      double assets = 0;
      double profits = 0;
      double points = 0;

      if (opens && closes) {
        assets = rsAssets_assets(model_assets(model, f, opens, closes));
        profits = rsProfits_sel(model_profits(model, f, opens, closes));
      }

      if (assets > assets_mx) assets_mx = assets;
      if (assets < assets_mn) assets_mn = assets;
      if (profits > profits_mx) profits_mx = profits;
      if (profits < profits_mn) profits_mn = profits;
      int df = date_df(now, date_from_str(flea_date(f)));
      if (df > days_mx) days_mx = df;

      arr_push(
        pool_ev,
        rankEvalEntry_new(mdname, f, assets, profits, df, points)
      );
    }
  }_EACH

  double assets_rk = assets_mx - assets_mn;
  double profits_rk = profits_mx - profits_mn;
  EACH(pool_ev, RankEvalEntry, e) {
    double assets_nm = (rankEvalEntry_assets(e) - assets_mn) / assets_rk;
    double profits_nm = (rankEvalEntry_profits(e) - profits_mn) / profits_rk;
    double days_nm = rankEvalEntry_days(e) / days_mx;
    rankEvalEntry_set_points(e,
      assets_nm * RANKING_ASSETS_RATIO +
      profits_nm * RANKING_PROFITS_RATIO +
      days_nm * RANKING_AGE_RATIO
    );
  }_EACH

  int fn (RankEvalEntry *e1, RankEvalEntry *e2) {
    return rankEvalEntry_points(e2) > rankEvalEntry_points(e1);
  }
  arr_sort(pool_ev, (FCMP)fn);

  pool_ev = arr_take(pool_ev, RANKING_POOL_NUMBER);
  arr_reverse(pool_ev);

  return pool_ev;
}

// Return Arr<RankEntry>, pool_ev is Arr<RankEvalEntry>
static Arr *to_rank_entries (Arr *pool_ev) {
  // Arr<RankEntry>
  Arr *pool = arr_new();

  EACH(pool_ev, RankEvalEntry, e) {
    arr_push(pool, rankEntry_new(
      rankEvalEntry_model_name(e),
      rankEvalEntry_flea(e),
      rankEvalEntry_assets(e),
      rankEvalEntry_points(e) * 1000
    ));
  }_EACH

  return pool;
}

// Rank ------------------------------------------------------------------------

// return Map<Arr<RankEntry>>
static Map *read_ranking (void) {
  if (!rankdb) EXC_ILLEGAL_STATE("'pool.db' was not intiliazed")

  // Return Arr<RankEntry>
  Arr *fn(Js *js) {
    return arr_from_js(js, (FFROM)rankEntry_from_js);
  }
  Js *js = (Js *)file_read(rankdb);
  return map_from_js(js, (FFROM)fn);
}

// rank is Arr<RankEntry>
static void save_rank(Arr *rank) {
  char *now = opt_nget(quotes_last_date());
  if (!now) now = date_to_str(date_now());

  Map *m = read_ranking();
  map_put(m, now, rank);

  Arr *keys = map_keys(m);
  arr_sort(keys, (FCMP)str_greater);
  arr_reverse(keys);
  while (map_size(m) > HISTORIC_RANKING_ENTRIES) {
    map_remove(m, arr_pop(keys));
  }

  // a is Arr<RankEntry>
  Js *fn(Arr *a) {
    return arr_to_js(a, (FTO)rankEntry_to_js);
  }
  char *s = (char *)map_to_js(m, (FTO)fn);

  file_write(rankdb, s);
}

// return Arr<RankEntry>
static Arr *last_ranking (void) {
  // Map<Arr<RankEntry>>
  Map *m = read_ranking();

  if (map_size(m)) {
    // Arr<char>
    Arr *keys = map_keys(m);
    arr_sort(keys, (FCMP)str_greater);
    return opt_get(map_get(m, arr_peek(keys)));
  }

  return arr_new();
}

// return Arr<RankEntry> descendingly sorted.
// pool_ev is Arr<RankEvalEntry>, rank is Arr<RankEntry>
static Arr *new_rank (Arr *pool_ev, Arr *rank) {
  // Arr<RankEvalEntry>
  Arr *r = arr_new();

  EACH(rank, RankEntry, e) {
    char *flname = flea_name(rankEntry_flea(e));
    char *mdname = rankEntry_model_name(e);
    int fn (RankEvalEntry *eve) {
      return
        str_eq(flname, flea_name(rankEvalEntry_flea(eve))) &&
        str_eq(mdname, rankEvalEntry_model_name(eve))
      ;
    }
    int ix = arr_index(pool_ev, (FPRED)fn);
    if (ix != -1) {
      arr_push(r, arr_get(pool_ev, ix));
      arr_remove(pool_ev, ix);
    }
  }_EACH

  int fn (RankEvalEntry *e1, RankEvalEntry *e2) {
    return rankEvalEntry_points(e2) > rankEvalEntry_points(e1);
  }
  arr_sort(r, (FCMP)fn);
  r = arr_take(r, RANKING_NUMBER * 0.9);

  while (arr_size(r) < RANKING_NUMBER)
    arr_push(r, arr_pop(pool_ev));

  arr_sort(r, (FCMP)fn);
  RankEntry *fmap(RankEvalEntry *e) {
    return rankEntry_new(
      rankEvalEntry_model_name(e),
      rankEvalEntry_flea(e),
      rankEvalEntry_assets(e),
      rankEvalEntry_points(e) * 1000
    );
  }
  return arr_map(r, (FCOPY)fmap);
}


// Public interface ------------------------------------------------------------

void rank_init () {
  drank = path_cat(io_data_dir(), "fleas", "_rank", NULL);
  pooldb = path_cat(drank, "pool.db", NULL);
  rankdb = path_cat(drank, "ranks.db", NULL);
  if (!file_exists(drank)) {
    file_mkdir(drank);
    file_write(pooldb, "[]");
    file_write(rankdb, "[]");
  }
}

void rank_update (void) {
  update_pool(); // Add new fleas
  // Arr<RankEntry>
  Arr *pool = read_pool();
  // Arr<RankEvalEntry> ascendingly
  Arr *pool_ev = evaluate(pool); // Evaluate and filter fleas.
  pool = to_rank_entries(pool_ev);
  save_pool(pool);
  // Arr<RankEntry> descendingly
  Arr *rank = last_ranking();
  rank = new_rank(pool_ev, rank);
  save_rank(rank);
}

Arr *rank_dates (void) {
  // Arr<char>
  Arr *dates = map_keys(read_ranking());
  if (!arr_size(dates))
    EXC_ILLEGAL_STATE("There is not any historic ranking group")

  arr_sort(dates, (FCMP)str_greater);
  return dates;
}

Arr *rank_fleas (char *date) {
  // Map<Arr<RankEntry>>
  Map *m =read_ranking();
  if (!map_size(m))
    EXC_ILLEGAL_STATE("There is not any historic ranking group")

  // Arr<RankEntry> descendingly
  return opt_oget(map_get(m, date), kv_value((Kv *)*arr_start(map_kvs(m))));
}

Arr *rank_fleas_previous (char *date) {
  // Map<Arr<RankEntry>>
  Map *m = read_ranking();
  int sz = map_size(m);
  if (!sz) return arr_new();

  // Arr<Kv<RankEntry>>
  Arr *kvs = map_kvs(m);

  int fsort (Kv *e1, Kv *e2) {
    return str_greater(kv_key(e2), kv_key(e1));
  }
  arr_sort(kvs, (FCMP)fsort);
  int fn (Kv *kv) {
    return str_eq(kv_key(kv), date);
  }
  int ix = arr_index(kvs, (FPRED)fn);
  if (ix == -1) ix = 0;
  ++ix;

  if (ix >= sz) return arr_new();
  return kv_value((Kv *)arr_get(kvs, ix));
}
