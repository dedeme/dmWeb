// Copyright 17-Oct-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "dmc/Js.h"
#include <ctype.h>
#include <locale.h>
#include <errno.h>
#include "dmc/std.h"

static void json_unicode(Buf *bf, char *hexdigits) {
  char hexvalue (char ch) {
    return (ch <= '9') ? ch - '0' : toupper(ch) - 'A' + 10;
  }
  unsigned int codepoint =
    hexvalue(*hexdigits) * 4096 +
    hexvalue(*(hexdigits + 1)) * 256 +
    hexvalue(*(hexdigits + 2)) * 16 +
    hexvalue(*(hexdigits + 3));
  if (codepoint < 0x80) {
    buf_cadd(bf, (codepoint>>0  & 0x7F) | 0x00);
    return;
  }
  if (codepoint < 0x0800) {
    buf_cadd(bf, (codepoint>>6  & 0x1F) | 0xC0);
    buf_cadd(bf, (codepoint>>0  & 0x3F) | 0x80);
    return;
  }
  buf_cadd(bf, (codepoint>>12 & 0x0F) | 0xE0);
  buf_cadd(bf, (codepoint>>6  & 0x3F) | 0x80);
  buf_cadd(bf, (codepoint>>0  & 0x3F) | 0x80);
}

static char *json_blanks(char *json) {
  while (*json && *json <= ' ') {
    ++json;
  }
  return json;
}

inline
static char *json_snull(char *json) {
  return json_blanks(json + 4);
}

static char *json_sbool(char *json) {
  if (*json == 't') {
    return json_blanks(json + 4);
  }
  return json_blanks(json + 5);
}

static char *json_snumber(char *j) {
  while (*j && *j != '}' && *j != ']' && *j != ',' && *j > ' ') {
    ++j;
  }
  return json_blanks(j);
}

static char *json_sstring(char *j) {
  ++j;
  while (*j && *j != '"') {
    if (*j++ == '\\') {
      if (*j) {
        ++j;
      }
    }
  }
  if (*j) {
    ++j;
  }
  return json_blanks(j);
}

static char *json_selement(char *j);

static char *json_sarray(char *j) {
  ++j;
  j = json_blanks(j);
  while (*j && *j != ']') {
    j = json_selement(j);
    if (*j == ',') {
      ++j;
      j = json_blanks(j);
    }
  }
  if (*j) {
    ++j;
  }
  return json_blanks(j);
}

static char *json_sobject(char *j) {
  ++j;
  j = json_blanks(j);
  while (*j && *j != '}') {
    j = json_sstring(j);
    ++j;
    j = json_selement(j);
    if (*j == ',') {
      ++j;
      j = json_blanks(j);
    }
  }
  if (*j) {
    ++j;
  }
  return json_blanks(j);
}

static char *json_selement(char *j) {
  j = json_blanks(j);
  switch (*j) {
  case 'n': return json_snull(j);
  case 't':
  case 'f': return json_sbool(j);
  case '"': return json_sstring(j);
  case '[': return json_sarray(j);
  case '{': return json_sobject(j);
  case '-': return json_snumber(j);
  default :
    if (*j >= '0' && *j <= '9') {
      return json_snumber(j);
    } else {
      if (*j) {
        ++j;
      }
      return j;
    }
  }
}

static int json_rend(char *json) {
  while (*json && *json <= ' ') {
    ++json;
  }
  return *json ? 0 : 1;
}

static char *json_error_new(char *json, char *msg) {
  return str_f_new("%s in\n'%s'\n", msg, json);
}

int js_is_null (Js *json) {
  char *j = json_blanks((char *)json);
  if (memcmp(j, "null", 4)) {
    return 0;
  }
  return json_rend(j + 4);
}

int js_rb (Js *json) {
  char *jsons = json_blanks((char *)json);
  int r;

  char *j = jsons;
  if (memcmp(j, "true", 4)) {
    if (memcmp(j, "false", 5)) {
      FAIL(json_error_new(jsons, "Bad value reading a boolean value"))
    }
    r = 0;
    j += 5;
  } else {
    r = 1;
    j += 4;
  }
  if (!json_rend(j)) {
    FAIL(json_error_new(jsons, "Spare characters reading a boolean value"))
  }
  return r;
}


int js_ri (Js *json) {
  char *jsons = json_blanks((char *)json);
  char *j = jsons;
  if (*j != '-' && (*j < '0' || *j > '9')) {
    FAIL(json_error_new(j, "Bad start number"))
  }
  Buf *bf = buf_new();
  while (*j && *j != '}' && *j != ']' && *j != ',' && *j > ' ') {
    ++j;
  }
  buf_add_buf(bf, jsons, j - jsons);
  char *n = buf_to_str_new(bf);
  buf_free(bf);
  if (!json_rend(j)) {
    FAIL(json_error_new(j, "Spare characters reading a number value"))
  }
  char *tail;
  errno = 0;
  int r = strtol(n, &tail, 10);
  if (errno) {
    FAIL(json_error_new(n, "Overflow"))
  }
  if (*tail) {
    FAIL(json_error_new(n, "Bad number"))
  }
  free(n);
  return r;
}

double js_rd (Js *json) {
  struct lconv *lc = localeconv();
  char *jsons = json_blanks((char *)json);
  char *j = jsons;
  if (*j != '-' && (*j < '0' || *j > '9')) {
    FAIL(json_error_new(jsons, "Bad start number"))
  }
  Buf *bf = buf_new();
  while (*j && *j != '}' && *j != ']' && *j != ',' && *j > ' ') {
    ++j;
  }
  buf_add_buf(bf, jsons, j - jsons);
  char *n = buf_to_str_new(bf);
  buf_free(bf);
  int ix = str_cindex(n, '.');
  if (ix != -1) {
    n[ix] = *lc->decimal_point;
  }
  if (!json_rend(j)) {
    FAIL(json_error_new(jsons, "Spare characters reading a number value"))
  }
  errno = 0;
  char *tail;
  double r = strtod(n, &tail);
  if (errno) {
    FAIL(json_error_new(n, "Overflow"))
  }
  if (*tail) {
    FAIL(json_error_new(jsons, "Bad number"))
  }
  free(n);
  return r;
}

char *js_rs_new (Js *j) {
  int is_hex (char ch) {
    return (ch >= '0' && ch <= '9') ||
      (ch >= 'a' && ch <= 'f') ||
      (ch >= 'A' && ch <= 'F');
  }

  char *json = json_blanks((char *)j);
  if (*json != '"') {
    FAIL(json_error_new(json, "String does not start with \""))
  }
  ++json;
  Buf *bf = buf_new();
  while (*json && *json != '"') {
    if (*json == '\\') {
      ++json;
      switch (*json) {
      case '"' :
      case '\\' :
      case '/' :
        buf_cadd(bf, *json);
        break;
      case 'b' :
        buf_cadd(bf, '\b');
        break;
      case 'f' :
        buf_cadd(bf, '\f');
        break;
      case 'n' :
        buf_cadd(bf, '\n');
        break;
      case 'r' :
        buf_cadd(bf, '\r');
        break;
      case 't' :
        buf_cadd(bf, '\t');
        break;
      case 'u' : {
        ++json;
        int c = 5;
        while (--c) {
          if (!is_hex(*json++)) {
            FAIL(json_error_new((char *)j, "Bad hexadecimal unicode in string"))
          }
        }
        json_unicode(bf, json - 4);
        continue;
      }
      default :
        FAIL(json_error_new((char *)j, "Bad escape sequence in string"))
      }
      ++json;
    } else {
      buf_cadd(bf, *json++);
    }
  }
  if (!*json) {
    FAIL(json_error_new((char *)j, "String does not end with \""))
  }
  if (!json_rend(json + 1)) {
    FAIL(json_error_new((char *)j, "Spare characters reading a string value"))
  }
  char *r = buf_to_str_new(bf);
  buf_free(bf);
  return r;
}

// Arr[Js]
Arr *js_ra_new (Js *j) {
  char *json = json_blanks((char *)j);
  if (*json != '[') {
    FAIL(json_error_new(json, "Array does not start with ["))
  }
  ++json;
  // Arr[Js]
  Arr *a = arr_new(free);
  while (*json && *json != ']') {
    char *tmp = json;
    json = json_selement(json);
    Buf *bf = buf_new();
    buf_add_buf(bf, tmp, json - tmp);
    arr_push(a, buf_to_str_new(bf));
    buf_free(bf);
    if (*json == ',') {
      ++json;
    } else if (*json && *json != ']') {
      FAIL(json_error_new((char *)j, "Comma missing reading an array value"))
    }
  }
  if (!*json) {
    FAIL(json_error_new((char *)j, "Array does not end with ]"))
  }
  if (!json_rend(json + 1)) {
    FAIL(json_error_new((char *)j, "Spare characters reading an array value"))
  }
  return a;
}

// Map[Js]
Map *js_ro_new (Js *j) {
  char *json = json_blanks((char *)j);
  if (*json != '{') {
    FAIL(json_error_new((char *)j, "Object does not start with {"))
  }
  json = json_blanks(json + 1);
  // Map[Js]
  Map *m = map_new(free);
  while (*json && *json != '}') {
    if (*json != '"') {
      FAIL(json_error_new((char *)j, "Object key is not a string"))
    }
    char *tmp = json;
    json = json_sstring(json);
    Buf *kbf = buf_new();
    buf_add_buf(kbf, tmp, json - tmp);

    if (*json != ':') {
      FAIL(json_error_new((char *)j, "key-value separator (:) is missing"))
    }
    ++json;
    tmp = json;
    json = json_selement(json);
    Buf *vbf = buf_new();
    buf_add_buf(vbf, tmp, json - tmp);

    tmp = buf_to_str_new(kbf);
    char *stmp = js_rs_new((Js *)tmp);
    map_put(m, stmp, (Js *)buf_to_str_new(vbf));
    free(stmp);
    free(tmp);
    buf_free(kbf);
    buf_free(vbf);

    if (*json == ',') {
      json = json_blanks(json + 1);
    } else if (*json && *json != '}') {
      FAIL(json_error_new((char *)j, "Comma missing reading an object value"))
    }
  }
  if (!*json) {
    FAIL(json_error_new((char *)j, "Object does not end with }"))
  }
  if (!json_rend(json + 1)) {
    FAIL(json_error_new((char *)j, "Spare characters reading an object value"))
  }
  return m;
}

Js *js_wn_new() {
  return (Js *)str_new("null");
}

Js *js_wb_new(int value) {
  return (Js *)str_new(value ? "true" : "false");
}

Js *js_wi_new(int n) {
  return (Js *)str_f_new("%d", n);
}

Js *js_wd_new(double n, int scale) {
  struct lconv *lc = localeconv();
  scale = (scale < 0) ? 0 : (scale > 9) ? 9 : scale;
  char format[] = "%.xf";
  format[2] = '0' + scale;
  char *ns = str_f_new(format, n);
  int ix = str_cindex(ns, *lc->decimal_point);
  if (ix != -1) {
    ns[ix] = '.';
  }
  return (Js *)ns;
}

Js *js_ws_new(const char *s) {
  Buf *bf = buf_new();

  char tmp;
  buf_cadd(bf, '"');
  while (*s) {
    tmp = *s++;
    switch (tmp) {
    case '"':
      buf_add(bf, "\\\"");
      break;
    case '\\':
      buf_add(bf, "\\\\");
      break;
    case '\b':
      buf_add(bf, "\\b");
      break;
    case '\f':
      buf_add(bf, "\\f");
      break;
    case '\n':
      buf_add(bf, "\\n");
      break;
    case '\r':
      buf_add(bf, "\\r");
      break;
    case '\t':
      buf_add(bf, "\\t");
      break;
    default:
      buf_cadd(bf, tmp);
    }
  }
  buf_cadd(bf, '"');
  Js *r = (Js *)buf_to_str_new(bf);
  buf_free(bf);
  return r;
}

// a = Arr[Js]
Js *js_wa_new(Arr *a) {
  int size = arr_size(a);
  Buf *bf = buf_new();
  buf_cadd(bf, '[');
  if (size > 0) {
    buf_add(bf, (char *)arr_get(a, 0));
    int i;
    for (i = 1; i < size; ++i) {
      buf_cadd(bf, ',');
      buf_add(bf, (char *)arr_get(a, i));
    }
  }
  buf_cadd(bf, ']');
  Js *r = (Js *)buf_to_str_new(bf);
  buf_free(bf);
  return r;
}

// m = Map[Js]
Js *js_wo_new(Map *m) {
  Buf *bf = buf_new();
  buf_cadd(bf, '{');
  EACH_IX(m, Kv, kv, i) {
    if (i) {
      buf_cadd(bf, ',');
    }
    char *tmp = (char *)js_ws_new(map_key(kv));
    buf_add(bf, tmp);
    free(tmp);
    buf_cadd(bf, ':');
    buf_add(bf, map_value(kv));
  }_EACH
  buf_cadd(bf, '}');
  Js *r = (Js *)buf_to_str_new(bf);
  buf_free(bf);
  return r;
}
