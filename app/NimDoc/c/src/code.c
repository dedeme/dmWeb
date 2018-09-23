// Copyright 10-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "code.h"

enum st_t {CODE, COMMENT, QUOTE1, QUOTE2, QUOTE3};

char *reserved = " addr and as asm block break case cast const continue "
  "converter discard div elif else end enum except exception false finally "
  "for from generic if implies import in include is isnot iterator lambda "
  "macro method mod nil not notin object of or out proc ptr raise ref "
  "return shl shr template true try tuple type var let when where while "
  "with without xor yield ";

char *reserved2 = " echo assert doAssert ord inc dec succ pred constains "
  "card inc excl write writeLine realdAll readLine isMainModule varargs "
  "result bool int int8 int16 int32 int64 uint uint8 uint16 uint32 uint64 "
  "float float32 float64 char string cstring pointer range set array seq "
  "openArray ";

static bool is_id_start(char ch) {
  return (ch >= 'a' && ch <= 'z') ||
    (ch >= 'A' && ch <= 'Z');
}

static bool is_digit(char ch) {
  return ch >= '0' && ch <= '9';
}

static bool is_id_cont(char ch) {
  return is_id_start(ch) || is_digit(ch) || ch == '_';
}


static char *rfile(LckFile *file, char *link) {
  int nlink = str_eq(link, "hp:") ? 0 : atoi(link);
  enum st_t st = CODE;
  Buf *bf = buf_new();

  char *l = file_read_line(file);
  int nl = 0;
  while (*l) {
    ++nl;
    if (nl == nlink) {
      buf_add(bf, "<span id = 'hp'>");
    }
    char *p = l;
    char ch = *p++;
    while (ch) {
      if (ch == '<') {
        buf_add(bf, "&lt;");
        ch = *p++;
        continue;
      }
      if (ch == '&') {
        buf_add(bf, "&amp;");
        ch = *p++;
        continue;
      }
      if (st == COMMENT) {
        if (ch == '\n') {
          buf_add(bf, "</span>\n");
          st = CODE;
        } else {
          buf_cadd(bf, ch);
        }
        ch = *p++;
        continue;
      }
      if (st == QUOTE1 || st == QUOTE2) {
        if (ch == '\\') {
          buf_cadd(bf, ch);
          buf_cadd(bf, *p++);
          ch = *p++;
          continue;
        }
        char q = '"';
        if (st == QUOTE2) {
          q = '\'';
        }
        if (ch == q) {
          buf_add(bf, str_printf("%c</span>", ch));
          st = CODE;
          ch = *p++;
          continue;
        }
        buf_cadd(bf, ch);
        ch = *p++;
        continue;
      }
      if (st == QUOTE3) {
        if (ch == '"' && *p == '"' && *(p + 1) == '"') {
          buf_add(bf, "\"\"\"</span>");
          st = CODE;
          ++p;
          ++p;
          ch = *p++;
          continue;
        }
        buf_cadd(bf, ch);
        ch = *p++;
        continue;
      }
      if (ch == '#') {
        st = COMMENT;
        ch = *p++;
        if (ch == '#') {
          buf_add(bf, "<span class='docComment'>##");
          ch = *p++;
          continue;
        }
        buf_add(bf, "<span class='comment'>#");
        continue;
      }
      if (ch == '"') {
        if (*p == '"' && *(p + 1) == '"') {
          buf_add(bf, "<span class = 'quote3'>\"\"\"");
          st = QUOTE3;
          ++p;
          ++p;
          ch = *p++;
          continue;
        }
        buf_add(bf, "<span class = 'quote1'>\"");
        st = QUOTE1;
        ch = *p++;
        continue;
      }
      if (ch == '\'') {
        buf_add(bf, "<span class = 'quote2'>'");
        st = QUOTE2;
        ch = *p++;
        continue;
      }
      if (is_id_start(ch)) {
        Buf *bf2 = buf_new();
        buf_cadd(bf2, ch);
        ch = *p++;
        while (is_id_cont(ch)) {
          buf_cadd(bf2, ch);
          ch = *p++;
        }
        char *id = buf_str(bf2);
        bool is_reserved = str_index(reserved, str_printf(" %s ", id)) != -1;
        bool is_reserved2 = str_index(reserved2, str_printf(" %s ", id)) != -1;
        bool is_type = *id >= 'A' && *id <= 'Z';
        if (is_reserved) {
          buf_add(bf, "<span class='reserved'>");
        }
        if (is_reserved2) {
          buf_add(bf, "<span class='reserved2'>");
        }
        if (is_type) {
          buf_add(bf, "<span class='className'>");
        }
        buf_add(bf, id);
        if (is_reserved || is_reserved2 || is_type) {
          buf_add(bf, "</span>");
        }
        continue;
      }

      if (is_digit(ch)) {
        Buf *bf2 = buf_new();
        buf_cadd(bf2, ch);
        ch = *p++;
        while (is_digit(ch)) {
          buf_cadd(bf2, ch);
          ch = *p++;
        }
        char *dg = buf_str(bf2);
        buf_add(bf, "<span class='number'>");
        buf_add(bf, dg);
        buf_add(bf, "</span>");
        continue;
      }

      buf_cadd(bf, ch);
      ch = *p++;
    }
    l = file_read_line(file);
  }
  return buf_to_str(bf);
}

CgiRp *code_process(Mjson *rq) {
  char *path = jmap_gstring(rq, "path");
  char *link = jmap_gstring(rq, "link");
  if (!file_exists(path)) {
    return cgi_ok(mjson_new());
  }
  LckFile *lck = file_ropen(path);
  char *html = rfile(lck, link);
  file_close(lck);
  Mjson *m = mjson_new();
  jmap_pstring(m, "html", html);
  return cgi_ok(m);
}
