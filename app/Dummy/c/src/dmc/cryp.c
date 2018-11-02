// Copyright 17-Oct-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "dmc/cryp.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "dmc/str.h"
#include "dmc/b64.h"
#include "dmc/DEFS.h"
#include "dmc/Buf.h"
#include "dmc/rnd.h"
#include "dmc/sys.h"

static char *b64_base =
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

char *cryp_genk_new (int lg) {
  if (lg <= 0) {
    FAIL("cryp_genk_new: lg < 0")
  }

  int len = strlen(b64_base);
  char *r = malloc(lg + 1);
  char *p = r + lg;
  *p-- = 0;
  while (lg--) {
    *p-- = *(b64_base + rnd_i(len));
  }
  return r;
}

void cryp_key (char **key, int lg) {
  if (!*key) {
    FAIL("cryp_key: keys is a blank string")
  }

  char *k0 = str_f_new(
    "%scodified in irreversibleDeme is good, very good!\n\r8@@", *key);
  char *b64 = b64_encode_new(k0);
  Bytes *k = b64_decode_bytes_new(b64);
  free(k0);
  free(b64);

  unsigned char *ka = bytes_bs(k);
  size_t lenk = bytes_len(k);
  unsigned char sum = 0;
  unsigned char *p = ka;
  REPEAT(lenk) {
    sum += *p++;
  }_REPEAT

  size_t lg2 = lg + lenk;
  Bytes *rbs = bytes_new2(lg2);
  unsigned char *ra = bytes_bs(rbs);
  Bytes *rbs1 = bytes_new2(lg2);
  unsigned char *ra1 = bytes_bs(rbs1);
  Bytes *rbs2 = bytes_new2(lg2);
  unsigned char *ra2 = bytes_bs(rbs2);

  size_t ik = 0;
  size_t v1, v2, v3, v4;
  for (size_t i = 0; i < lg2; ++i) {
    v1 = ka[ik];
    v2 = v1 + ka[v1 % lenk];
    v3 = v2 + ka[v2 % lenk];
    v4 = v3 + ka[v3 % lenk];
    sum = sum + i + v4;
    ra1[i] = sum;
    ra2[i] = sum;
    ++ik;
    if (ik == lenk) {
      ik = 0;
    }
  }

  for (size_t i = 0; i < lg2; ++i) {
    v1 = ra2[i];
    v2 = v1 + ra2[v1 % lg2];
    v3 = v2 + ra2[v2 % lg2];
    v4 = v3 + ra2[v3 % lg2];
    sum = sum + v4;
    ra2[i] = sum;
    ra[i] = sum + ra1[i];
  }

  char *r = b64_encode_bytes_new(rbs);
  str_left(&r, lg);
  free(*key);
  *key = r;

  bytes_free(k);
  bytes_free(rbs);
  bytes_free(rbs1);
  bytes_free(rbs2);
}

void cryp_cryp (char **s, const char *k) {
  if (!*k) {
    FAIL("cryp_cryp: k is a blank string")
  }

  char *b64 = b64_encode_new(*s);

  size_t lg = strlen(b64);
  char *k2 = str_new(k);
  cryp_key(&k2, lg);

  Bytes *rbs = bytes_new2(lg);

  unsigned char *prbs = bytes_bs(rbs);
  unsigned char *pk2 = (unsigned char *)k2;
  unsigned char *pb64 = (unsigned char *)b64;
  REPEAT(lg) {
    *prbs++ = (*pk2++) + (*pb64++);
  }_REPEAT

  free(*s);
//puts(b64_encode_bytes_new(rbs));
//puts("-------------");
  *s = b64_encode_bytes_new(rbs);
  free(b64);
  free(k2);
  bytes_free(rbs);
}

void cryp_decryp (char **c, const char *k) {
  if (!*k) {
    FAIL("cryp_cryp: k is a blank string")
  }

  Bytes *bs = b64_decode_bytes_new(*c);

  size_t lg = bytes_len(bs);
  char *k2 = str_new(k);
  cryp_key(&k2, lg);

  char *b64 = malloc(lg + 1);

  unsigned char *pbs = bytes_bs(bs);
  unsigned char *pk2 = (unsigned char *)k2;
  char *pb64 = b64;
  REPEAT(lg) {
    *pb64++ = *pbs++ - *pk2++;
  }_REPEAT
  *pb64 = 0;

  free(*c);
  *c = b64_decode_new(b64);

  free(k2);
  free(b64);
  bytes_free(bs);
}

void cryp_auto_cryp (char **s, int nk) {
  nk = nk < 1 ? 0 : nk > 64 ? 63 : nk - 1;
  char *n = str_c_new(b64_base[nk]);
  char *k = cryp_genk_new(nk + 1);
  cryp_cryp(s, k);
  char *r = str_cat_new(n, k, *s, NULL);
  free(*s);
  *s = r;

  free(n);
  free(k);
}

void cryp_auto_decryp (char **b64) {
  char *s = *b64;
  int nk = str_cindex(b64_base, *s) + 1;
  Buf *bf = buf_new();
  buf_add_buf(bf, s + 1, nk);
  char *key = buf_to_str_new(bf);
  str_right(b64, nk + 1);
  cryp_decryp(b64, key);

  buf_free(bf);
  free(key);
}

void cryp_encode (char **s, int nk, const char *k) {
  cryp_auto_cryp(s, nk);
  cryp_cryp(s, k);
}

void cryp_decode (char **b64, const char *k) {
  cryp_decryp(b64, k);
  cryp_auto_decryp(b64);
}
