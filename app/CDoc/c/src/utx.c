// Copyright 21-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "utx.h"
#include "dmc/Tx.h"

int utx_is_l (char ch) {
  return (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || ch == '_';
}

int utx_is_ld (char ch) {
  return utx_is_l(ch) || (ch >= '0' && ch <= '9');
}

char *utx_mk_link (char *s) {
  Tx *tx = tx_new(s);
  int ix;
  char ch;
  tx_cindexes(&ix, &ch, tx, "\n;=(");
  ix = ix == -1 ? tx_length(tx) : ix;
  if (ch == '(' && tx_starts(tx_right(tx, ix), "(*")) {
    int ix2 = tx_cindex(tx_right(tx, ix + 1), '(');
    ix = ix2 == -1 ? ix : ix + ix2 + 1;
  }
  char *r = str_creplace(tx_to(tx_left(tx, ix)), '#', '_');
  Buf *bf = buf_new();
  REPEAT(strlen(r)) {
    char ch = *r++;
    if (ch > ' ') buf_cadd(bf, ch);
  }_REPEAT
  return buf_to_str(bf);
}

char *utx_read_name (char *tx, int i) {
  char ch = tx[i];
  while (ch && !utx_is_l(ch)) ch = tx[++i];
  if (ch) {
    int start = i;
    while (utx_is_ld(tx[++i]));
    return str_sub(tx, start, i);
  }
  return "???";
}

char *utx_read_namebk (char *tx, int i) {
  --i;
  while (i >= 0 && !utx_is_ld(tx[i])) --i;
  if (i >= 0) {
    int end = i + 1;
    --i;
    while (i >= 0 && utx_is_ld(tx[i])) --i;
    return str_sub(tx, i + 1, end);
  }
  return "???";
}
