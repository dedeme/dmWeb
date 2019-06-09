// Copyright 24-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "net.h"
#include "dmc/ext.h"
#include "dmc/Dec.h"
#include "dmc/date.h"
#include "io/log.h"
#include "io/servers.h"
#include "io/quotes.h"
#include "io/nicks.h"
#include "data/DailyEntry.h"
#include "data/HistoricEntry.h"
#include "data/Server.h"
#include "DEFS.h"

static char *wget (char *url) {
  char *r = "";
  REPEAT(3)
    r = ext_wget(url);
    if (*r) break;
  _REPEAT
  return r;
}

static int to_double(double *ret, char *n) {
  int errno = 0;
  char *tail;
  *ret = strtod(n, &tail);
  if (errno || *tail)  {
    if (errno) log_error(str_f("Bad number, error: %s", strerror(errno)));
    else log_error(str_f("Extra characters '%s' in number", tail));
    return 0;
  }
  if (*ret < 0) {
    log_error(str_f("Negative quote: '%f'", *ret));
    return 0;
  }
  return 1;
}

// Returns Opt[char]
static Opt *mkdate(char *d, char *sep, int is_eu) {
  // Arr[char]
  Arr *parts = str_csplit(d, *sep);
  if (arr_size(parts) != 3) return opt_empty();
  char *day = is_eu ? arr_get(parts, 0) : arr_get(parts, 1);
  char *month = is_eu ? arr_get(parts, 1) : arr_get(parts, 0);
  char *year = arr_get(parts, 2);
  if (!dec_digits(day)) return opt_empty();
  if (!dec_digits(month)) return opt_empty();
  if (!dec_digits(year)) return opt_empty();
  int lday = strlen(day);
  int lmonth = strlen(month);
  int lyear = strlen(year);
  if (lday < 1 || lday > 2) return opt_empty();
  if (lmonth < 1 || lmonth > 2) return opt_empty();
  if (lyear != 4 && lyear != 2) return opt_empty();
  if (lday == 1) day = str_cat("0", day, NULL);
  if (lmonth == 1) month = str_cat("0", month, NULL);
  if (lyear == 2) year = str_cat("20", year, NULL);
  return opt_new(str_cat(year, month, day, NULL));
}

// marks is Arr[char]
static int goStart (char *html, int start, Arr *marks) {
  EACH(marks, char, m)
    start = str_index_from(html, m, start);
    if (start == -1) break;
    start += strlen(m);
  _EACH
  return start;
}

// marks is Arr[char]
static int goEnd (char *html, int start, Arr *marks) {
  int len = 0;
  EACH(marks, char, m)
    start = str_index_from(html, m, start + len);
    if (start == -1) break;
    len = strlen(m);
  _EACH
  return start;
}

// Reads a conf returning an Opt[Arr[Arr[char]]].<p>
// Fails are written in Log.
//   conf: Can be a historic or daily configuration
//   code: If 'conf' is a historic configuation, 'code' has the company code.
//         Otherwise 'code' is an empty string.
static Opt *read (Rconf *conf, char *code) {
  char *url = rconf_url(conf);
  if (*code) {
    url = str_replace(url, "${code}", code);
  }
  char *html = wget(url);
  if (!*html) {
    log_error(str_f("Inet error reading '%s'", url));
    return opt_empty();
  }

  int start = goStart(html, 0, str_csplit(rconf_table_start(conf), '|'));
  if (start == -1) {
    log_error(str_f("Start table not found reading '%s'", url));
    return opt_empty();
  }
  int end = goEnd(html, start, str_csplit(rconf_table_end(conf), '|'));
  if (end == -1) {
    log_error(str_f("End table not found reading '%s'", url));
    return opt_empty();
  }
  html = str_sub(html, start, end);

  Arr *row_start = str_csplit(rconf_row_start(conf), '|'); // Arr[char]
  Arr *row_end = str_csplit(rconf_row_end(conf), '|'); // Arr[char]
  Arr *cols_start = arr_new(); // Arr[Arr[char]]
  EACH(rconf_cols_start(conf), char, mks)
    arr_push(cols_start, str_csplit(mks, '|'));
  _EACH
  Arr *cols_end = arr_new(); // Arr[Arr[char]]
  EACH(rconf_cols_end(conf), char, mks)
    arr_push(cols_end, str_csplit(mks, '|'));
  _EACH

  // Arr[Arr[char]]
  Arr *rows = arr_new();
  start = 0;
  while (1) {
    start = goStart(html, start, row_start);
    if (start == -1) break;
    end = goEnd(html, start, row_end);
    if (end == -1) break;

    char *htmlr = str_sub(html, start, end);
    // Arr[char]
    Arr *cells = arr_new();
    int cstart = 0;
    RANGE0(i, arr_size(cols_start))
      cstart = goStart(htmlr, cstart, arr_get(cols_start, i));
      if (cstart == -1) break;
      int cs = cstart;
      cstart = goEnd(htmlr, cstart, arr_get(cols_end, i));
      if (cstart == -1) break;
      arr_push(cells, str_trim(str_sub(htmlr, cs, cstart)));
    _RANGE
    if (cstart != -1) {
      arr_push(rows, cells);
    }

    start = end;
  }
  return opt_new(rows);
}

// Returns Opt[Arr[DailyEntry]]
Opt *net_read_daily (Rconf *conf) {
  // Opt[Arr[Arr[char]]]
  Opt *table = read(conf, "");
  if (opt_is_empty(table)) return table;

  char *types = rconf_fields_type(conf);
  int ixC = str_cindex(types, 'C');
  int ixQ = str_cindex(types, 'Q');
  // Arr[DailyEntry]
  Arr *t = arr_new();
  EACH(opt_get(table), Arr, row) // row is Arr[char]
    char *code = arr_get(row, ixC);
    char *close = arr_get(row, ixQ);
    close = rconf_is_iso_number(conf)
      ? dec_regularize_iso(close)
      : dec_regularize_us(close)
    ;
    if (dec_number(close)) {
      double nclose;
      if (to_double(&nclose, close)) arr_push(t, dailyEntry_new(code, nclose));
    }
  _EACH

  return opt_new(t);
}

// Returns Opt[Arr[HistoricEntry]]
Opt *net_read_historic (Rconf *conf, char *code) {
  // Opt[Arr[Arr[char]]]
  Opt *table = read(conf, code);
  if (opt_is_empty(table)) return opt_empty();

  char *types = rconf_fields_type(conf);
  int ixD = str_cindex(types, 'D');
  int ixO = str_cindex(types, 'O');
  int ixC = str_cindex(types, 'C');
  int ixX = str_cindex(types, 'X');
  int ixN = str_cindex(types, 'N');
  int ixV = str_cindex(types, 'V');

  // Arr[DailyEntry]
  Arr *t = arr_new();
  EACH(opt_get(table), Arr, row) // row is Arr[char]
    // Opt[char]
    Opt *date = mkdate(
      arr_get(row, ixD),
      rconf_date_separator(conf),
      rconf_is_date_eu(conf)
    );
    char *open = arr_get(row, ixO);
    char *close = arr_get(row, ixC);
    char *max = arr_get(row, ixX);
    char *min = arr_get(row, ixN);
    char *vol = arr_get(row, ixV);

    if (rconf_is_iso_number(conf)) {
      open = dec_regularize_iso(open);
      close = dec_regularize_iso(close);
      max = dec_regularize_iso(max);
      min = dec_regularize_iso(min);
      vol = dec_regularize_iso(vol);
    } else {
      open = dec_regularize_us(open);
      close = dec_regularize_us(close);
      max = dec_regularize_us(max);
      min = dec_regularize_us(min);
      vol = dec_regularize_us(vol);
    }

    if (
      opt_is_full(date) &&
      dec_number(open) &&
      dec_number(close) &&
      dec_number(max) &&
      dec_number(min) &&
      dec_digits(vol)
    ) {
      double nopen;
      double nclose;
      double nmax;
      double nmin;
      int nvol = atoi(vol);

      if (
        to_double(&nopen, open) &&
        to_double(&nclose, close) &&
        to_double(&nmax, max) &&
        to_double(&nmin, min) &&
        nvol >= 0
      ) {
        arr_push(t, historicEntry_new(
          opt_get(date), nopen, nclose, nmax, nmin, nvol
        ));
      }
    }
  _EACH

  return opt_new(t);
}

EMsg *net_update_historic(int nk_id) {
  // Opt[Nick]
  Opt *onk = nicks_get(nk_id);
  if (opt_is_empty(onk)) {
    log_error(str_f("Nick id '%d' not found", nk_id));
    return eMsg_new(MSG_ERROR, "nick");
  }
  Nick *nk = opt_get(onk);

  // Arr[Quote]
  Arr *old_qs = quotes_read(nick_name(nk));
  if (!arr_size(old_qs)) {
    log_error(str_f("Wrong quotes in '%s.db'", nick_name(nk)));
    return eMsg_new(MSG_ERROR, "nick");
  }
  // old_qs is not empty

  int model_id = nicks_model();
  if (model_id == -1) {
    log_error("Nick model not defined");
    return eMsg_new(MSG_ERROR, "model");
  }

  // Arr[Quote]
  Arr *model_qs = arr_new();
  if (nk_id != model_id) {
    char *model_name = nick_name(opt_eget(nicks_get(model_id), str_f(
      "Nick model with id '%d' not found", model_id
    )));
    model_qs = quotes_read(model_name);
    if (!arr_size(model_qs)) {
      log_error("Nick model quotes are wrong");
      return eMsg_new(MSG_ERROR, "model");
    }
  }

  EMsg *err = eMsg_new(MSG_OK, "");
  // Arr[Server]
  Arr *servers = servers_list();
  // Arr[Arr[Quote]]
  Arr *server_historics = arr_new();
  // Arr[Quote]
  Arr *best_qs = arr_new();
  EACH(servers, Server, sv)
    // Opt[Rconf]
    Opt *oconf = server_historic_conf(sv);
    if (opt_is_empty(oconf)) {
      continue;
    }
    Rconf *conf = opt_get(oconf);
    int ffind(ServerCode *sc) { return serverCode_nick_id(sc) == nk_id; }
    // Opt[ServerCode]
    Opt *sc = it_find(arr_to_it(server_codes(sv)), (FPRED)ffind);
    if (opt_is_empty(sc)) {
      log_error(str_f(
        "Nick id of '%s' not found in server '%s'",
        nick_name(nk), server_name(sv)
      ));
      if (eMsg_error(err) != MSG_ERROR) err = eMsg_new(MSG_ERROR, "server");
    }
    // Opt[char]
    Opt *code = serverCode_code((ServerCode *)opt_get(sc));
    if (opt_is_empty(code)) {
      log_error(str_f(
        "Nick code of '%s' not found in server '%s'",
        nick_name(nk), server_name(sv)
      ));
      if (eMsg_error(err) != MSG_ERROR) err = eMsg_new(MSG_ERROR, "server");
    }

    // Opt[Arr[HistoricEntry]]
    Opt *hentries = net_read_historic(conf, opt_get(code));
    if (opt_is_empty(hentries)) {
      log_error(str_f(
        "Quotes of '%s' not read from the Internet using server '%s'",
        nick_name(nk), server_name(sv)
      ));
      if (eMsg_error(err) != MSG_ERROR) err = eMsg_new(MSG_ERROR, "net");
    }
    char *limit_date = date_to_str(date_add(date_now(), -29));
    // Arr[Quote]
    Arr *qs = arr_new();
    EACH(opt_get(hentries), HistoricEntry, he)
      if (strcmp(historicEntry_date(he), limit_date) < 0) {
        break;
      }
      arr_push(qs, quote_new(
        historicEntry_date(he),
        historicEntry_open(he),
        historicEntry_close(he),
        historicEntry_max(he),
        historicEntry_min(he),
        historicEntry_vol(he),
        0
      ));
    _EACH
    if (arr_size(qs)) { // qs is not empty
      arr_push(server_historics, qs);
      if (rconf_sel(conf)) {
        best_qs = qs;
      }
    }
  _EACH

  // Arr[Quote]
  Arr *new_qs = arr_new();
  if (arr_size(server_historics)) {
    if (arr_size(server_historics) == 1) {
      new_qs = arr_get(server_historics, 0);
    } else {
      if (!arr_size(best_qs)) {
        best_qs = arr_get(server_historics, 0);
      }
      new_qs = quote_unify(server_historics, best_qs);
    }
  } else {
    if (eMsg_error(err) != MSG_ERROR) err = eMsg_new(MSG_ERROR, "net");
    return err; // new_qs is empty
  }
  // new_qs is not empty

  // Tp[Arr[char], Arr[Quote]]
  Tp *e_qs = quote_blend(model_qs, new_qs, old_qs);

  // Arr[char]
  Arr *e = tp_e1(e_qs);
  // Arr[Quote]
  Arr *qs = tp_e2(e_qs);
  if (arr_size(e)) {
    log_error(str_f(
      "Wrong quotes in '%s.db':\n%s", nick_name(nk), str_cjoin(e, '\n')
    ));
    if (eMsg_error(err) != MSG_ERROR) {
      err = eMsg_new(MSG_WARNING, "quotes");
    } else {
      err = eMsg_new(MSG_ERROR, str_f("%s/quotes", eMsg_msg(err)));
    }
  }

  quotes_write(nick_name(nk), qs);

  return err;
}
