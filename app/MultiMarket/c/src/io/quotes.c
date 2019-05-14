// Copyright 10-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "io/quotes.h"
#include "io.h"
#include "DEFS.h"

char *quotes_db = NULL;

void quotes_init (void) {
  quotes_db = path_cat(io_data_dir(), "quotes", NULL);
  if (!file_exists(quotes_db)) {
    file_mkdir(quotes_db);
  }
}

Kv *quotes_read(char *nick_name) {
  if (!quotes_db)
    EXC_IO("'quotes.db' was not initialized")
  char *fpath = path_cat(quotes_db, str_f("%s.db", nick_name), NULL);

  // Arr[Quote]
  Arr *qs = arr_new();

  if (!file_exists(fpath)) return kv_new(quotes_IO, qs);
  // Arr[char *]
  Arr *lines = str_csplit(str_trim(file_read(fpath)), '\n');

  if (!arr_size(lines)) return kv_new(quotes_EMPTY, qs);

  if (arr_size(lines) < HISTORIC_QUOTES) return kv_new(quotes_MISSING, qs);

  EACH_IX(lines, char, l, ix)
    if (ix == HISTORIC_QUOTES) break;

    Opt *q = quote_from_str(l);
    if (opt_is_empty(q)) {
      qs = arr_new();
      break;
    }
    arr_push(qs, opt_get(q));

  _EACH

  if (arr_size(qs)) return kv_new(quotes_OK, qs);

  return kv_new(quotes_SYNTAX, qs);
}

char *quotes_write(char *nick_name, Arr *qs) {
  if (!quotes_db)
    EXC_IO("'quotes.db' was not initialized")
  char *fpath = path_cat(quotes_db, str_f("%s.db", nick_name), NULL);

  if (!arr_size(qs)) return quotes_EMPTY;

  if (arr_size(qs) < HISTORIC_QUOTES) return quotes_MISSING;

  // Arr[char]
  Arr *lines = arr_new();
  EACH_IX(qs, Quote, q, ix)
    if (ix == HISTORIC_QUOTES) break;
    arr_push(lines, quote_to_str(q));
  _EACH

  file_write(fpath, str_cjoin(lines, '\n'));
  return quotes_OK;
}

void quotes_add(char *nick_name) {
  if (!quotes_db)
    EXC_IO("'quotes.db' was not initialized")
  char *fpath = path_cat(quotes_db, str_f("%s.db", nick_name), NULL);

  if (!file_exists(fpath)) {
    file_write(fpath, "");
  }
}

void quotes_del(char *nick_name) {
  if (!quotes_db)
    EXC_IO("'quotes.db' was not initialized")
  char *fpath = path_cat(quotes_db, str_f("%s.db", nick_name), NULL);

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
