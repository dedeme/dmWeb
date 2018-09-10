// Copyright 10-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "code.h"
#include "data/Creader.h"

enum st_t {CODE, LONG_COMMENT, SHORT_COMMENT, QUOTE1, QUOTE2, QUOTE3};

char *reserved = " class arguments await break case catch const continue "
    "debugger default delete do else enum eval export extends false finally "
    "for from function if implements import in instanceof interface let new "
    "null of package private protected public return static super switch "
    "this throw true try typeof var void while with yield constructor self ";

char *rfile(LckFile *file, char *link) {
  enum st_t st = CODE;
  Buf *bf = buf_new();
  Creader *reader = creader_new(file, link);

  char *lk = creader_next_line(reader);
  while (!creader_end(reader)) {
    if (*lk) {
      buf_add(bf, str_printf("<span id = \"%s\">", lk));
    }
    char ch = creader_peek_char(reader);
    while (ch) {
      if (ch == '<') {
        buf_add(bf, "&lt;");
        ch = creader_next_char(reader);
        continue;
      }
      if (ch == '&') {
        buf_add(bf, "&amp;");
        ch = creader_next_char(reader);
        continue;
      }
      if (st == LONG_COMMENT) {
        if (ch == '*') {
          char *tx = creader_end_comment(reader);
          if (*tx) {
            buf_add(bf, tx);
            st = CODE;
            ch = creader_peek_char(reader);
            continue;
          }
        }
        buf_cadd(bf, ch);
        ch = creader_next_char(reader);
        continue;
      }
      if (st == SHORT_COMMENT) {
        if (ch == '\n') {
          buf_add(bf, "</span>\n");
          st = CODE;
        } else {
          buf_cadd(bf, ch);
        }
        ch = creader_next_char(reader);
        continue;
      }
      if (st == QUOTE1 || st == QUOTE2 || st == QUOTE3) {
        if (ch == '\\') {
          buf_cadd(bf, ch);
          buf_cadd(bf, creader_next_char(reader));
          ch = creader_next_char(reader);
          continue;
        }
        char q = '"';
        if (st == QUOTE2) {
          q = '\'';
        } else if (st == QUOTE3) {
          q = '`';
        }
        if (ch == q) {
          buf_add(bf, str_printf("%c</span>", ch));
          st = CODE;
          ch = creader_next_char(reader);
          continue;
        }
        buf_cadd(bf, ch);
        ch = creader_next_char(reader);
        continue;
      }
      if (ch == '/') {
        char *tx = creader_start_long_doc(reader);
        if (*tx) {
          buf_add(bf, tx);
          st = LONG_COMMENT;
          ch = creader_peek_char(reader);
          continue;
        }
        tx = creader_start_long_comment(reader);
        if (*tx) {
          buf_add(bf, tx);
          st = LONG_COMMENT;
          ch = creader_peek_char(reader);
          continue;
        }
        tx = creader_start_short_doc(reader);
        if (*tx) {
          buf_add(bf, tx);
          st = SHORT_COMMENT;
          ch = creader_peek_char(reader);
          continue;
        }
        tx = creader_start_short_comment(reader);
        if (*tx) {
          buf_add(bf, tx);
          st = SHORT_COMMENT;
          ch = creader_peek_char(reader);
          continue;
        }
      }
      if (ch == '"') {
        buf_add(bf, "<span class = 'quote1'>\"");
        st = QUOTE1;
        ch = creader_next_char(reader);
        continue;
      }
      if (ch == '\'') {
        buf_add(bf, "<span class = 'quote2'>'");
        st = QUOTE2;
        ch = creader_next_char(reader);
        continue;
      }
      if (ch == '`') {
        buf_add(bf, "<span class = 'quote3'>`");
        st = QUOTE3;
        ch = creader_next_char(reader);
        continue;
      }
      char *tx = creader_id(reader);
      if (*tx) {
        bool is_reserved = str_index(reserved, str_printf(" %s ", tx)) != -1;
        bool is_class = *tx >= 'A' && *tx <= 'Z';
        if (is_reserved) {
          buf_add(bf, "<span class='reserved'>");
        }
        if (is_class) {
          buf_add(bf, "<span class='className'>");
        }
        buf_add(bf, tx);
        if (is_reserved || is_class) {
          buf_add(bf, "</span>");
        }
        ch = creader_peek_char(reader);
        continue;
      }
      tx = creader_number(reader);
      if (*tx) {
        buf_add(bf, "<span class='number'>");
        buf_add(bf, tx);
        buf_add(bf, "</span>");
        ch = creader_peek_char(reader);
        continue;
      }
      buf_cadd(bf, ch);
      ch = creader_next_char(reader);
    }
    lk = creader_next_line(reader);
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
