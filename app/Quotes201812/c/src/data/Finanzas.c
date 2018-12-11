// Copyright 10-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Finanzas.h"
#include "data/Quote.h"
#include "data/Close.h"
#include "dmc/ext.h"
#include "dmc/Dec.h"
#include "dmc/Date.h"

static char *url_base =
  "http://www.finanzas.com/cotizaciones/$/datos-historicos.html";

static char *url_last = "http://www.finanzas.com/mercado-continuo/";

static char *path(char *code) {
  return str_replace(url_base, "$", code);
}

// Read --------------------------------------------------------------

static int read_td(char **s, char *tx, int ix) {
  ix = str_index_from(tx, "<td", ix);
  if (ix == -1) return 0;
  ix = str_cindex_from(tx, '>', ix);
  if (ix == -1) return 0;
  ++ix;
  int i2 = str_cindex_from(tx, '<', ix);
  if (i2 == -1) return 0;
  *s = str_sub(tx, ix, i2);

  return i2;
}

static int read_tdd(char **date, char *tx, int ix) {
  char *s;
  ix = read_td(&s, tx, ix);
  if (ix == -1) return 0;
  Date d = date_from_iso_sep(s, '/');
  if (!d) return 0;
  *date = date_to_str(d);
  return ix;
}

static int read_tdf(double *q, char *tx, int ix) {
  char *s;
  ix = read_td(&s, tx, ix);
  if (ix) {
    char *reg = dec_regularize_iso(s);
    if (dec_number(reg)) {
      *q = atof(reg);
      return ix;
    }
  }
  return 0;
}

static int read_tdi(int *q, char *tx, int ix) {
  char *s;
  ix = read_td(&s, tx, ix);
  if (ix) {
    char *reg = dec_regularize_iso(s);
    if (dec_number(reg)) {
      *q = atoi(reg);
      return ix;
    }
  }
  return 0;
}

static void process_row(Aquote *r, char *tx, int ix) {
  char *null;
  char *date;
  double open, close, max, min;
  int vol;
  ix = read_tdd(&date, tx, ix);
  if (ix == -1) return;
  ix = read_tdf(&open, tx, ix);
  if (ix == -1) return;
  ix = read_tdf(&close, tx, ix);
  if (ix == -1) return;
  ix = read_td(&null, tx, ix);
  if (ix == -1) return;
  ix = read_tdf(&max, tx, ix);
  if (ix == -1) return;
  ix = read_tdf(&min, tx, ix);
  if (ix == -1) return;
  ix = read_tdi(&vol, tx, ix);
  if (ix == -1) return;
  aquote_add(r, quote_new(date, open, close, max, min, vol, false));
}

static Oaquote *process_table(char *tx, int ix, int end) {
  Aquote *r = aquote_new();
  int i2;
  for(;;) {
    ix = str_index_from(tx, "<tr", ix);
    if (ix == -1 || ix > end) return oaquote_new(r);
    i2 = str_index_from(tx, "</tr>", ix);
    if (i2 == -1) return oaquote_null();
    process_row(r, tx, ix);
    ix = i2;
  }
}

static Oaquote *process_page(char *pg) {
  int ix = str_index(pg, "<h2>Hist");
  if (ix == -1) return oaquote_null();
  ix = str_index_from(pg, "<tbody>", ix);
  if (ix == -1) return oaquote_null();
  int i2 = str_index_from(pg, "</table>", ix);
  if (i2 == -1) return oaquote_null();
  return process_table(pg, ix, i2);
}

static Oaquote *read(char *code) {
  Achar *apage = ext_wget(path(code));
  char *page = str_cjoin(achar_to_it(apage), '\n');
  return process_page(page);
}

// Read last ---------------------------------------------------------

static int read_tdn(char **name, char *tx, int ix) {
  ix = str_index_from(tx, "/cotizaciones/", ix);
  if (ix == -1) return 0;
  ix += 14;
  int i2 = str_cindex_from(tx, '/', ix);
  if (i2 == -1) return 0;
  *name = str_sub(tx, ix, i2);
  return i2;
}

static void process_row_last(Aclose *closes, char *tx, int ix) {
  char *name;
  double close;
  ix = read_tdn(&name, tx, ix);
  if (!ix) return;

  if (read_tdf(&close, tx, ix)) {
    aclose_add(closes, close_new(name, close));
  }
  return;
}

static Oaclose *process_table_last(char *tx, int ix, int end) {
  Aclose *r = aclose_new();
  int i2;
  for(;;) {
    ix = str_index_from(tx, "<tr>", ix);
    if (ix == -1 || ix > end) return oaclose_new(r);
    i2 = str_index_from(tx, "</tr>", ix);
    if (i2 == -1) return oaclose_null();
    process_row_last(r, tx, ix);
    ix = i2;
  }
}

static Oaclose *process_last(char *pg) {
  int ix = str_index(pg, "mod-valores-completo");
  if (ix == -1) {
    return oaclose_null();
  }
  ix = str_index_from(pg, "<tbody>", ix);
  if (ix == -1) return oaclose_null();
  int i2 = str_index_from(pg, "</tbody>", ix);
  if (i2 == -1) return oaclose_null();
  return process_table_last(pg, ix, i2);
}

static Oaclose *read_last() {
  Achar *apage = ext_wget(url_last);
  char *page = str_cjoin(achar_to_it(apage), '\n');
  return process_last(page);
}

Server *finanzas_mk(void) {
  return server_new("Finanzas", path, read, read_last);
}
