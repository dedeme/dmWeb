// Copyright 17-Oct-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// B64 encoder - decoder

#ifndef DMC_B64_H
  #define DMC_B64_H

#include "dmc/Bytes.h"

///
char *b64_decode_new(const char *b64);

///
Bytes *b64_decode_bytes_new(const char *b64);

///
char *b64_encode_new(const char *s);

///
char *b64_encode_bytes_new(Bytes *bs);

#endif
