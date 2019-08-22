// Copyright 20-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "io/modulerd.h"
#include "utx.h"
#include "dmc/Tx.h"

typedef struct modulerd_DocRs DocRs;
struct modulerd_DocRs {
  char *doc;
  char *line;
};
static DocRs *docRs_new (char *doc, char *line) {
  DocRs *this = MALLOC(DocRs);
  this->doc = doc;
  this->line = line;
  return this;
}

// Returns Opt[DocRs]
static Opt *read_doc (FileLck *f, char *l) {
  char *find_line (char *l) {
    while (*l) {
      if (*str_ltrim(l)) return l;
      l = file_read_line(f);
    }
    return "";
  }

  for (;;) {
    Buf *bf = buf_new();
    char *ltrim = str_ltrim(l);

    if (str_starts(ltrim, "///")) {
      for (;;) {
        char ch = ltrim[3];
        buf_add(bf, ch == ' ' || ch == '\t'
          ? str_right(ltrim, 4)
          : str_right(ltrim, 3)
        );
        l = file_read_line(f);
        if (!*l) break;
        ltrim = str_ltrim(l);
        if (!str_starts(ltrim, "///")) break;
      }
      l = find_line(l);
      if (*str_ltrim(l) == '/') continue;
      else return opt_new(docRs_new(buf_to_str(bf), l));
    }

    if (str_starts(ltrim, "/**")) {
      char *l = file_read_line(f);
      char *ltrim = str_ltrim(l);
      if (!*ltrim || str_index(ltrim, "*/") != -1)
        return opt_empty();
      int strip = strlen(l) - strlen(ltrim);

      for (;;) {
        buf_add(bf, ltrim);
        l = file_read_line(f);
        if (!*l || str_index(l, "*/") != - 1)
          break;
        ltrim = strlen(l) < strip ? "" : str_right(l, strip);
      }
      l = find_line(file_read_line(f));
      if (*str_ltrim(l) == '/') continue;
      else return opt_new(docRs_new(buf_to_str(bf), l));
    }

    return opt_empty();
  }
}

static char *read_code (FileLck *f, char *line, int is_first) {
  char *l = str_ltrim(line);

  if (is_first && str_starts(l, "#ifndef "))
    return "overview";
  Buf *bf = buf_new();
  if (str_starts(l, "#define ")) {
    for (;;) {
      buf_add(bf, l);
      if (str_ends(l, "\\\n")) {
        l = file_read_line(f);
        if (*l) continue;
      }
      return buf_to_str(bf);
    }
  }

  if (*l == '#') return "";

  int with_brackets = 0;
  while (*line) {
    Tx *tx = tx_new(line);
    if (!with_brackets) {
      int ix;
      char ch;
      tx_cindexes(&ix, &ch, tx, ";{");
      if (ix == -1) {
        buf_add(bf, line);
        line = file_read_line(f);
        continue;
      }
      ++ix;
      buf_add(bf, str_left(line, ix));
      if (ch == ';') {
        return buf_to_str(bf);
      }
      line = str_right(line, ix);
      with_brackets = 1;
      continue;
    }
    int ix;
    char ch;
    tx_cindexes(&ix, &ch, tx, "{}");
    if (ix == -1) {
      buf_add(bf, line);
      line = file_read_line(f);
      continue;
    }
    ++ix;
    buf_add(bf, str_left(line, ix));
    with_brackets = with_brackets + (ch == '{' ? 1 : -1);
    line = str_right(line, ix);
  }

  return "";
}

Doc *modulerd_read (char *path) {
  FileLck *f = file_ropen(path);
  int is_first = 1;
  Doc *doc = doc_new("");

  for (;;) {
    char *l = file_read_line(f);
    if (*str_ltrim(l) == '/') {
      DocRs *rs = opt_nget(read_doc(f, l));
      if (rs && *rs->line) {
        char *cd = read_code(f, rs->line, is_first);
        char *cdtrim = str_trim(cd);
        is_first = 0;
        if (str_eq(cd, "overview")) {
          doc = doc_new(rs->doc);
        } else if (str_starts(cd, "#define ")) {
          arr_push(doc_defines(doc), docEntry_new(
            utx_read_name(cdtrim, strlen("#define ")),
            rs->doc, cd, utx_mk_link(cdtrim)
          ));
        } else if (str_starts(cd, "typedef ")) {
          int ix = str_cindex(cdtrim, ')');
          if (ix == -1) ix = strlen(cdtrim);
          arr_push(doc_typedefs(doc), docEntry_new(
            utx_read_namebk(cdtrim, ix),
            rs->doc, cd, utx_mk_link(cdtrim)
          ));
        } else if (str_cindex(cdtrim, '{') != -1) {
          if (str_starts(cd, "enum ")) {
            arr_push(doc_enums(doc), docEntry_new(
              utx_read_name(cdtrim, strlen("enum ")),
              rs->doc, cd, utx_mk_link(cdtrim)
            ));
          }
          if (str_starts(cd, "struct ")) {
            arr_push(doc_structs(doc), docEntry_new(
              utx_read_name(cdtrim, strlen("struct ")),
              rs->doc, cd, utx_mk_link(cdtrim)
            ));
          }
          if (str_starts(cd, "union ")) {
            arr_push(doc_unions(doc), docEntry_new(
              utx_read_name(cdtrim, strlen("union ")),
              rs->doc, cd, utx_mk_link(cdtrim)
            ));
          }
        } else {
          int ix = str_cindex(cdtrim, '=');
          if (ix != -1) {
            arr_push(doc_vars(doc), docEntry_new(
              utx_read_namebk(cdtrim, ix),
              rs->doc, cd, utx_mk_link(cdtrim)
            ));
          } else {
            ix = str_cindex(cdtrim, '(');
            if (ix != -1) {
              if (str_starts(str_trim(str_right(cdtrim, ix + 1)), "*"))
                ix = str_cindex_from(cdtrim, ix, ')');
              arr_push(doc_functions(doc), docEntry_new(
                utx_read_namebk(cdtrim, ix),
                rs->doc, cd, utx_mk_link(cdtrim)
              ));
            } else if (*cdtrim) {
              arr_push(doc_vars(doc), docEntry_new(
                utx_read_namebk(cdtrim, strlen(cdtrim)),
                rs->doc, cd, utx_mk_link(cdtrim)
              ));
            }
          }
        }
      }
    } else if (!*l) {
      break;
    }
  }

  return doc;
}
