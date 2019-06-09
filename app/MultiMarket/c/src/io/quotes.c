// Copyright 10-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "io/quotes.h"
#include "io/nicks.h"
#include "io/log.h"
#include "io.h"
#include "data/Nick.h"
#include "DEFS.h"

char *quotes_db = NULL;

void quotes_init (void) {
  quotes_db = path_cat(io_data_dir(), "quotes", NULL);
  if (!file_exists(quotes_db)) {
    file_mkdir(quotes_db);
  }
}

// Arr[Quote]
Arr *quotes_read(char *nk_name) {
  if (!quotes_db)
    EXC_IO("'quotes.db' was not initialized")
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

  if (arr_size(model_qs)) {
    es_qs = quote_check_dates(model_qs, qs);
    es = tp_e1(es_qs);
    qs = tp_e2(es_qs);
    if (arr_size(es)) {
      log_error(str_f(
        "Missing/Extra quotes in '%s.db':\n  %s", nk_name, str_join(es, "\n  ")
      ));
      e = eMsg_new(MSG_WARNING, "quotes");
    }
  } else if (arr_size(qs) != HISTORIC_QUOTES) {
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
