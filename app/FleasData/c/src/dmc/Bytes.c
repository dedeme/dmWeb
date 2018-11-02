// Copyright 16-Oct-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "dmc/Bytes.h"
#include <string.h>
#include "dmc/std.h"
#include "dmc/b64.h"

struct bytes_Bytes {
  unsigned char *bs;
  int length;
};

inline
Bytes *bytes_new() {
  return bytes_new2(0);
}

Bytes *bytes_new2(int length) {
  Bytes *this = malloc(sizeof(Bytes));
  this->bs = malloc(length);
  this->length = length;
  return this;
}

Bytes *bytes_from_bytes_new(unsigned char *bs, int length) {
  Bytes *this = malloc(sizeof(Bytes));
  this->bs = malloc(length);
  memcpy(this->bs, bs, length);
  this->length = length;
  return this;
}

Bytes *bytes_from_str_new (const char *s) {
  return bytes_from_bytes_new((unsigned char *)s, strlen(s));
}

void bytes_free(Bytes *this) {
  free(this->bs);
  free(this);
}

unsigned char *bytes_bs(Bytes *this) {
  return this->bs;
}

int bytes_len(Bytes *this) {
  return this->length;
}

void bytes_add_bytes (Bytes *this, unsigned char *bs, int length) {
  int sum = this->length + length;
  if (sum) {
    this->bs = realloc(this->bs, sum);
    memcpy(this->bs + this->length, bs, length);
    this->length = sum;
  }
}

void bytes_add (Bytes *this, Bytes *another) {
  bytes_add_bytes(this, another->bs, another->length);
}

void bytes_add_str (Bytes *this, const char *s) {
  bytes_add_bytes(this, (unsigned char *)s, strlen(s));
}

Js *bytes_to_js_new(Bytes *this) {
  char *b64 = b64_encode_bytes_new(this);
  Js *r = js_ws_new(b64);
  free(b64);
  return r;
}

Bytes *bytes_from_js_new(Js *js) {
  char *b64 = js_rs_new(js);
  Bytes *r = b64_decode_bytes_new(b64);
  free(b64);
  return r;
}
