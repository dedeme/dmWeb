// Copyright 10-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Creader.h"

/* .+.
-struct: @Creader
  -lck: LckFile *
  -link: char *
  -
  -is_function: bool = false
  -is_top: bool = false
  -is_constructor: bool = false
  -is_get: bool = false
  -is_set: bool = false
  -link_class: char * = ""
  -link_id: char * = ""
  -line: char * = ""
  end: bool = false
  -class: char * = ""
*/

/*.-.*/
#include "dmc/ct/Ajson.h"

struct creader_Creader {
  LckFile *lck;
  char *link;

  bool is_function;
  bool is_top;
  bool is_constructor;
  bool is_get;
  bool is_set;
  char *link_class;
  char *link_id;
  char *line;
  bool end;
  char *class;
};

Creader *_creader_new(LckFile *lck, char *link) {
  Creader *this = MALLOC(Creader);
  XNULL(lck)
  this->lck = lck;
  XNULL(link)
  this->link = link;
  this->is_function = false;
  this->is_top = false;
  this->is_constructor = false;
  this->is_get = false;
  this->is_set = false;
  this->link_class = "";
  this->link_id = "";
  this->line = "";
  this->end = false;
  this->class = "";
  return this;
}

bool creader_end(Creader *this) {
  XNULL(this)
  return this->end;
}
/*.-.*/

static char *get_fn_name(char *line) {
  int ix = str_cindex(line, '(');
  if (ix != -1) {
    return str_rtrim(str_sub(line, 0, ix));
  }
  return "-";
}

static bool is_id_start(char ch) {
  return (ch >= 'a' && ch <= 'z') ||
    (ch >= 'A' && ch <= 'Z') ||
    ch == '_' || ch == '$'
  ;
}

static bool is_digit(char ch) {
  return ch >= '0' && ch <= '9';
}

static bool is_id_cont(char ch) {
  return is_id_start(ch) || is_digit(ch);
}

// ________________
// public interface --------------------------------------------------
// TTTTTTTTTTTTTTTT

Creader *creader_new(LckFile *lck, char *link) {
  Creader *this = _creader_new(lck, link);
  link = str_sub_end(link, 3);
  int ix = str_cindex(link, '.');
  if (ix == -1) {
    this->is_top = true;
    this->link_id = link;
  } else {
    this-> link_class = str_sub(link, 0, ix);
    link = str_sub_end(link, ix);
    if (str_eq(link, ".")) {
      this->is_constructor = true;
    } else {
      link = str_sub_end(link, 1);
      if (str_ends(link, ".get")) {
        this->is_get = true;
        this->link_id = str_sub(link, 0, -4);
      } else if (str_ends(link, ".set")) {
        this->is_get = true;
        this->link_id = str_sub(link, 0, -4);
      } else {
        this->link_id = link;
      }
    }
  }

  return this;
}

char creader_next_char(Creader *this) {
  char r = *this->line;
  if (r) {
    ++this->line;
    return *this->line;
  }
  return r;
}

char *creader_next_line(Creader *this) {
  char *line = file_read_line(this->lck);
  this->line = line;
  if (!*line) {
    this->end = true;
    return "";
  }
  if (*line == '}') {
    this->class = "";
    return "";
  }
  if (str_starts(line, "export ")) {
    int ix = str_index(line, " class ");
    if (ix != -1) {
      char *rest = str_sub_end(line, ix + 7);
      int ix = str_index(rest, "{");
      if (ix != -1) {
        this->class = str_trim(str_sub(rest, 0, ix));
        if (this->is_top && str_eq(this->class, this->link_id)) {
          return this->link;
        }
      }
      return "";
    }

    if (this->is_top) {
      line = str_ltrim(str_sub_end(line, 7));
      if (str_starts(line, "function ")) {
        line = str_ltrim(str_sub_end(line, 9));
        if (str_eq(this->link_id, get_fn_name(line))) {
           return this->link;
        }
      }
    }
    return "";
  }

  if (str_eq(this->class, this->link_class)) {
    if (str_starts(line, "  ") && line[3] > ' ') {
      line = str_sub_end(line, 2);
      if (
        this->is_constructor &&
        ( str_starts(line, "constructor ") ||
          str_starts(line, "constructor("))
      ) {
        return this->link;
      }
      if (this->is_get) {
        if (str_starts(line, "get ")) {
          line = str_ltrim(str_sub_end(line, 4));
          if (str_eq(this->link_id, get_fn_name(line))) {
             return this->link;
          }
        }
        return "";
      }
      if (this->is_set) {
        if (str_starts(line, "set ")) {
          line = str_ltrim(str_sub_end(line, 4));
          if (str_eq(this->link_id, get_fn_name(line))) {
             return this->link;
          }
        }
        return "";
      }
      if (str_starts(line, "static ")) {
        line = str_ltrim(str_sub_end(line, 7));
      }
      if (str_eq(this->link_id, get_fn_name(line))) {
        return this->link;
      }
    }
  }
  return "";
}

char creader_peek_char(Creader *this) {
  return *this->line;
}

char *creader_start_long_comment(Creader *this) {
  if (str_starts(this->line, "/*")) {
    ++this->line;
    ++this->line;
    return "<span class='comment'>/*";
  }
  return "";
}

///
char *creader_start_long_doc(Creader *this) {
  if (str_starts(this->line, "/**") && this->line[3] != '/') {
    ++this->line;
    ++this->line;
    ++this->line;
    return "<span class='docComment'>/**";
  }
  return "";
}

///
char *creader_start_short_comment(Creader *this) {
  if (str_starts(this->line, "//")) {
    ++this->line;
    ++this->line;
    return "<span class='comment'>//";
  }
  return "";
}

///
char *creader_start_short_doc(Creader *this) {
  if (str_starts(this->line, "///")) {
    ++this->line;
    ++this->line;
    ++this->line;
    return "<span class='docComment'>///";
  }
  return "";
}

char *creader_end_comment(Creader *this) {
  if (str_starts(this->line, "*/")) {
    ++this->line;
    ++this->line;
    return "*/</span>";
  }
  return "";
}

char *creader_id(Creader *this) {
  if (!is_id_start(*this->line)) {
    return "";
  }
  char *p = this->line;
  while (is_id_cont(*p)) {
    ++p;
  }
  char *r = str_sub(this->line, 0, p - this->line);
  this->line = p;
  return r;
}

char *creader_number(Creader *this) {
  if (!is_digit(*this->line)) {
    return "";
  }
  char *p = this->line;
  while (is_digit(*p)) {
    ++p;
  }
  char *r = str_sub(this->line, 0, p - this->line);
  this->line = p;
  return r;
}

