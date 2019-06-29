// Copyright 10-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "io/quotes.h"
#include "io/nicks.h"
#include "io/log.h"
#include "io.h"
#include "data/Nick.h"
#include "DEFS.h"

static char *quotes_db = NULL;

void quotes_init (void) {
  quotes_db = path_cat(io_data_dir(), "quotes", NULL);
  if (!file_exists(quotes_db)) {
    file_mkdir(quotes_db);
  }
}

// Arr[Quote]
Arr *quotes_read(char *nk_name) {
  if (!quotes_db) EXC_IO("'quotes.db' was not initialized")

  char *fpath = path_cat(quotes_db, str_f("%s.db", nk_name), NULL);

  // Arr[Quote]
  Arr *qs = arr_new();

  if (!file_exists(fpath)) return qs;
  // Arr[char *]
  Arr *lines = str_csplit(str_trim(file_read(fpath)), '\n');

  if (arr_size(lines) != HISTORIC_QUOTES) return qs;

  EACH(lines, char, l)
    Opt *q = quote_from_str(l);
    if (opt_is_empty(q)) {
      return arr_new();
    }
    arr_push(qs, opt_get(q));
  _EACH

  return qs;
}

// qs is Arr[Quote]
void quotes_write(char *nk_name, Arr *qs) {
  if (!quotes_db)
    EXC_IO("'quotes.db' was not initialized")
  char *fpath = path_cat(quotes_db, str_f("%s.db", nk_name), NULL);

  // Arr[char]
  Arr *lines = arr_new();
  EACH(qs, Quote, q)
    arr_push(lines, quote_to_str(q));
  _EACH

  file_write(fpath, str_cjoin(lines, '\n'));
}

void quotes_add(char *nk_name) {
  if (!quotes_db)
    EXC_IO("'quotes.db' was not initialized")
  char *fpath = path_cat(quotes_db, str_f("%s.db", nk_name), NULL);

  if (!file_exists(fpath)) {
    file_write(fpath, "");
  }
}

void quotes_del(char *nk_name) {
  if (!quotes_db)
    EXC_IO("'quotes.db' was not initialized")
  char *fpath = path_cat(quotes_db, str_f("%s.db", nk_name), NULL);

  if (file_exists(fpath)) {
    file_del(fpath);
  }
}

void quotes_modify (char *old_name, char *new_name) {
  if (!quotes_db)
    EXC_IO("'quotes.db' was not initialized")
  char *opath = path_cat(quotes_db, str_f("%s.db", old_name), NULL);
  char *npath = path_cat(quotes_db, str_f("%s.db", new_name), NULL);

  if (file_exists(opath)) {
    file_rename(opath, npath);
  }
}

//return is Tp[EMsg, Arr[Quote]]. 'model_qs' is Arr[Quote]. It can be empty.
static Tp *check_qs (char *nk_name, char *text, Arr *model_qs) {
  // Arr[char *]
  Arr *lines = str_csplit(text, '\n');
  // Arr[Quote]
  Arr *qs = arr_new();
  // Arr[char]
  Arr *es = arr_new();
  EACH_IX(lines, char, l, ix)
    Opt *q = quote_from_str(l);
    if (opt_is_empty(q)) {
      arr_push(es, str_f("%d: %s", ix + 1, str_left(l, 35)));
    } else {
      arr_push(qs, opt_get(q));
    }
  _EACH
  if (arr_size(es)) {
    log_error(str_f(
      "Syntax error in '%s.db':\n  %s", nk_name, str_join(es, "\n  ")
    ));
    return tp_new(eMsg_new(MSG_ERROR, "syntax"), qs);
  }

  if (!arr_size(qs)) {
    log_error(str_f("'%s.db' has no quote", nk_name));
    return tp_new(eMsg_new(MSG_ERROR, "empty"), qs);
  }

  EMsg *e = eMsg_new(MSG_OK, "");

  if (arr_size(model_qs)) {
    Tp *es_qs = quote_check_dates(model_qs, qs);
    es = tp_e1(es_qs);
    qs = tp_e2(es_qs);
    if (arr_size(es)) {
      log_error(str_f(
        "Missing/Extra quotes in '%s.db':\n  %s", nk_name, str_join(es, "\n  ")
      ));
      e = eMsg_new(MSG_WARNING, "quotes");
    }
  }

  // Tp[Arr[char], Arr[Quote]]
  Tp *es_qs = quote_check(qs);
  es = tp_e1(es_qs);
  qs = tp_e2(es_qs);
  if (arr_size(es)) {
    log_error(str_f(
      "Wrong quotes in '%s.db':\n  %s", nk_name, str_join(es, "\n  ")
    ));
    e = eMsg_new(MSG_WARNING, "quotes");
  }

  if (arr_size(qs) != HISTORIC_QUOTES) {
    int size = arr_size(qs);
    if (size > HISTORIC_QUOTES) {
      arr_remove_range(qs, HISTORIC_QUOTES, size);
    }

    log_error(str_f("Quotes number of '%s.db' is different from %d",
      nk_name, HISTORIC_QUOTES
    ));
    if (eMsg_error(e) == MSG_OK) {
      e = eMsg_new(MSG_WARNING, "number");
    }
  }

  return tp_new(e, qs);
}

// Returns is Tp[EMsg, char]
Tp *quotes_editor_read(int nick_id) {
  char *nk_name = nick_name(opt_eget(nicks_get(nick_id), str_f(
    "Nick with id '%d' not found", nick_id
  )));

  if (!quotes_db)
    EXC_IO("'quotes.db' was not initialized")
  char *fpath = path_cat(quotes_db, str_f("%s.db", nk_name), NULL);

  if (!file_exists(fpath)) {
    log_error(str_f("'%s.db' not found", nk_name));
    return tp_new(eMsg_new(MSG_ERROR, "io"), "");
  }
  char *text = str_trim(file_read(fpath));

  int model_id = nicks_model();
  if (model_id == -1) {
    log_error("Nick model not defined");
    return tp_new(eMsg_new(MSG_ERROR, "model"), text);
  }

  // Arr[Quote]
  Arr *model_qs = arr_new();
  if (nick_id != model_id) {
    char *model_name = nick_name(opt_eget(nicks_get(model_id), str_f(
      "Nick model with id '%d' not found", model_id
    )));
    model_qs = quotes_read(model_name);
    if (!arr_size(model_qs)) {
      log_error("Nick model quotes are wrong");
      return tp_new(eMsg_new(MSG_ERROR, "model"), text);
    }
  }

  return tp_new(tp_e1(check_qs(nk_name, text, model_qs)), text);
}

EMsg *quotes_editor_set_quotes(int nick_id, char *qs_text) {
  int model_id = nicks_model();
  if (model_id == -1) {
    log_error("Nick model not defined");
    return eMsg_new(MSG_ERROR, "model");
  }

  // Arr[Quote]
  Arr *model_qs = arr_new();
  if (nick_id != model_id) {
    char *model_name = nick_name(opt_eget(nicks_get(model_id), str_f(
      "Nick model with id '%d' not found", model_id
    )));
    model_qs = quotes_read(model_name);
    if (!arr_size(model_qs)) {
      log_error("Nick model quotes are wrong");
      return eMsg_new(MSG_ERROR, "model");
    }
  }

  char *nk_name = nick_name(opt_eget(nicks_get(nick_id), str_f(
    "Nick with id '%d' not found", nick_id
  )));

  // Tp[eMsg, Arr[Quote]]
  Tp *e_qs = check_qs(nk_name, qs_text, model_qs);
  EMsg *e = tp_e1(e_qs);
  // Arr[Quote]
  Arr *qs = tp_e2(e_qs);
  if (eMsg_error(e) == MSG_ERROR) {
    return e;
  }

  quotes_write(nk_name, qs);

  return e;
}

Arr *quotes_dates (void) {
  int model_id = nicks_model();
  if (model_id == -1) {
    // Arr[Nick]
    Arr *nicks = nicks_list();
    if (arr_size(nicks) == 0) {
      log_error("Nick model not defined");
      return arr_new();
    }
    model_id = nick_id(arr_get(nicks, 0));
  }
  // Arr[Quote]
  Arr *qs = arr_new();
  char *model_name = nick_name(opt_eget(nicks_get(model_id), str_f(
    "Nick with id '%d' not found", model_id
  )));
  qs = quotes_read(model_name);
  int qs_size = arr_size(qs);
  if (qs_size != HISTORIC_QUOTES) {
    if (qs_size) {
      log_error(str_f(
        "%s:%d:[%s]:\n  Wrong quotes number of '%s': Expected %d. Actual %d"
        __FILE__, (char *)__func__, __LINE__,
        model_name, HISTORIC_QUOTES, qs_size
      ));
    } else {
      log_error(str_f(
        "%s:%d:[%s]:\n  Quotes of '%s' have errors"
        __FILE__, (char *)__func__, __LINE__, model_name
      ));
    }
    return arr_new();
  }

  // Arr[char]
  Arr *r = arr_new();
  EACHR(qs, Quote, q)
    arr_push(r, quote_date(q));
  _EACH
  return r;
}

// Returns Opt[Qmatrix]
static Opt *mk_qmtrix (double (*get_value)(Quote *)) {
  int ffilter (Nick *nk) { return nick_is_sel(nk); }
  // Arr[Nick]
  Arr *nicks = arr_from_it(it_filter(arr_to_it(nicks_list()), (FPRED)ffilter));
  int nicks_size = arr_size(nicks);

  QmatrixValues *rows = GC_MALLOC(HISTORIC_QUOTES * sizeof(QmatrixValues));
  QmatrixValues *p = rows;
  REPEAT(HISTORIC_QUOTES)
    *p++ = ATOMIC(nicks_size * sizeof(double));
  _REPEAT
  EACH_IX(nicks, Nick, nk, col)
    char *name = nick_name(nk);
    // Arr[Quote]
    Arr *qs = quotes_read(name);
    int qs_size = arr_size(qs);

    if (qs_size != HISTORIC_QUOTES) {
      if (qs_size) {
        log_error(str_f(
          "%s:%d:[%s]:\n  Wrong quotes number of '%s': Expected %d. Actual %d"
          __FILE__, (char *)__func__, __LINE__,
          name, HISTORIC_QUOTES, qs_size
        ));
      } else {
        log_error(str_f(
          "%s:%d:[%s]:\n  Quotes of '%s' have errors"
          __FILE__, (char *)__func__, __LINE__, name
        ));
      }
      return opt_empty();
    } else {
      arr_reverse(qs);
      Quote **quotes = (Quote **)arr_start(qs);
      RANGE0(row, HISTORIC_QUOTES)
        rows[row][col] = get_value(*quotes++);
      _RANGE
    }
  _EACH

  return opt_new(qmatrix_new(nicks, rows));
}

// Returns Opt[Qmatrix]
Opt *quotes_closes (void) {
  return mk_qmtrix(quote_close);
}

// Returns Opt[Qmatrix]
Opt *quotes_opens (void) {
  return mk_qmtrix(quote_open);
}

// Returns Opt[char]
Opt *quotes_last_date (void) {
  int model_id = nicks_model();
  if (model_id == -1) {
    // Arr[Nick]
    Arr *nicks = nicks_list();
    if (arr_size(nicks) == 0) {
      log_error("Nick model not defined");
      return opt_empty();
    }
    model_id = nick_id(arr_get(nicks, 0));
  }
  // Arr[Quote]
  Arr *model_qs = arr_new();
  char *model_name = nick_name(opt_eget(nicks_get(model_id), str_f(
    "Nick with id '%d' not found", model_id
  )));
  model_qs = quotes_read(model_name);
  if (!arr_size(model_qs)) {
    log_error("Nick model quotes are wrong");
    return opt_empty();
  }

  return opt_new(quote_date(arr_get(model_qs, 0)));
}

// Returns Map[Js->double]
Map *quotes_last_quotes (void) {
  if (!quotes_db) EXC_IO("'quotes.db' was not initialized")

  // Map [Js->double]
  Map *r = map_new();
  EACH(file_dir(quotes_db), char, f)
    char *nick_name = str_left(f, -3);
    // Arr[Quote]
    Arr *qs = quotes_read(nick_name);
    if (arr_size(qs) == 0) {
      log_error(str_f(
        "quotes_last_quotes: Error reading quotes of %s", nick_name
      ));
      continue;
    }
    double close = -1;
    EACH(qs, Quote, q)
      close = quote_close(q);
      if (close > 0) break;
    _EACH
    map_put(r, nick_name, js_wd(close));
  _EACH

  return r;
}

// Returns Map[Js->int]
Js *quotes_volume (void) {
  if (!quotes_db) EXC_IO("'quotes.db' was not initialized")

  // Map[Js]
  Map *r = map_new();
  EACH(file_dir(quotes_db), char, f)
    char *nick = str_left(f, -3);
    double sum = 0;
    int n = 0;
    EACH_IX(quotes_read(nick), Quote, q, ix)
      if (ix == VOLUME_QUOTES) break;
      double max = quote_max(q);
      double min = quote_min(q);
      int vol = quote_vol(q);
      if (max > 0 && min > 0 && vol >= 0) {
        sum += (quote_max(q) + quote_min(q)) * ((double)quote_vol(q)) / 2.0;
        ++n;
      }
    _EACH
    map_put(r, nick, js_wi((int)(sum / ((double)n))));
  _EACH

  return js_wo(r);
}
