// Copyright 25-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "net/as.h"
#include <stdlib.h>
#include "dmc/ext.h"
#include "dmc/Tx.h"
#include "dmc/err.h"
#include "dmc/str.h"
#include "data/cts.h"

static int get_team (Tx *t) {
  int ix = tx_cindex(t, '_');
  if (ix == -1)
    FAIL(str_f("'_' not found in\n%s", str_left(tx_to(t), 80)));

  t = tx_right(t, ix);
  ix = tx_cindex(t, '"');
  if (ix == -1)
    FAIL(str_f("'\"' not found in\n%s", str_left(tx_to(t), 80)));

  char *team = tx_to(tx_sub(t, 1, ix));
  /**/int findex(char *e) { return str_eq(e, team); }
  int r = achar_index(cts_as_teams(), findex);
  if (r == -1)
    FAIL(str_f("'%s' is not a valid team in\n%s", team, str_left(tx_to(t), 80)));
  return r;
}

static Result *get_result(Tx *t) {
  int ix = tx_index(t, "\"resultado\"");
  if (ix == -1)
    FAIL(str_f("'resultado' not found in\n%s", str_left(tx_to(t), 80)));

  t = tx_right(t, ix);
  ix = tx_cindex(t, '>');
  if (ix == -1)
    FAIL(str_f("'>' not found in\n%s", str_left(tx_to(t), 80)));

  t = tx_right(t, ix + 1);
  ix = tx_cindex(t, '-');
  if (ix == -1)
    FAIL(str_f("'-' not found in\n%s", str_left(tx_to(t), 80)));

  int ix2 = tx_cindex(t, '<');
  if (ix2 == -1)
    FAIL(str_f("'<' not found in\n%s", str_left(tx_to(t), 80)));
  if (ix2 < ix)
    FAIL(str_f("'<' is previous to '-' in\n%s", str_left(tx_to(t), 80)));

  return result_new(
    atoi(str_trim(tx_to(tx_left(t, ix)))),
    atoi(str_trim(tx_to(tx_sub(t, ix + 1, ix2))))
  );
}

AAOResult *as_read (void) {
  char *html = ext_wget(
    "https://resultados.as.com/resultados/futbol/primera/calendario/",
    0
  );

  Tx *t = tx_new(html);

  AAOResult *r = aAOResult_new_nones();

  for (;;) {
    int ix = tx_index(t, "col-equipo-local sel");
    if (ix == -1) break;
    t = tx_right(t, ix + 1);

    int home_team = get_team(t);

    ix = tx_index(t, "col-equipo-visitante");
    if (ix == -1)
      FAIL(str_f("Out team is missing in\n%s", tx_to(tx_left(t, 80))));

    Tx *txr = tx_left(t, ix);
    OResult *rs = oResult_mk_none();
    int ixr = tx_index(txr, "col-resultado finalizado");
    if (ixr != -1) {
      rs = oResult_mk_some(get_result(tx_right(t, ixr)));
    }

    t = tx_right(t, ix + 1);
    int out_team = get_team(t);

    aOResult_set(aAOResult_get(r, home_team), out_team, rs);
  }

  return r;
}
