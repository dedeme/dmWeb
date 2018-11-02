// Copyright 16-Oct-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "dmc/Buf.h"
#include "dmc/std.h"
#include "string.h"

struct buf_Buf{
  char *str;
  int length;
  int size;
};

Buf *buf_new(void) {
  return buf_bf_new(151);
}

Buf *buf_bf_new(int buffer_size) {
  Buf *this = malloc(sizeof(Buf));
  this->size = buffer_size - 1;
  this->str = malloc(buffer_size);
  memset(this->str, 0, buffer_size);
  this->length = 0;
  return this;
}

void buf_free(Buf *this) {
  free(this->str);
  free(this);
}

int buf_len(Buf *this) {
  return this->length;
}

char *buf_str(Buf *this) {
  return this->str;
}

void buf_add_buf (Buf *this, const char *data, int length) {
  int ixend = this->length + length;
  if (this->size < ixend) {
      while (this->size < ixend) {
          this->size += this->size;
      }
      int memsize = this->size + 1;
      char *newstr = malloc(memsize);
      memset(newstr, 0, memsize);
      memcpy(newstr, this->str, this->length);
      free(this->str);
      this->str = newstr;
  }
  memcpy(this->str + this->length, data, length);
  this->length = ixend;
}

void buf_add (Buf *this, const char *data) {
  buf_add_buf(this, data, strlen(data));
}

void buf_cadd (Buf *this, char data) {
  buf_add_buf(this, &data, 1);
}

char *buf_to_str_new (Buf *this) {
  return str_new(this->str);
}

void buf_reset(Buf *this) {
  memset(this->str, 0, this->size);
  this->length = 0;
}
