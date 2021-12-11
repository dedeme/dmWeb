// Copyright 11-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "readers/moduleRd.h"
#include "dmc/str.h"
#include "dmc/Buf.h"
#include "dmc/path.h"
#include "dmc/file.h"
#include "dmc/Tx.h"
#include "db/dpaths.h"
#include "data/Dpath/ADpath.h"
#include "data/Doc/ODoc.h"
#include "data/DocRs/ODocRs.h"
#include "readers/utx.h"

static ODocRs *read_doc (FileLck *f, char *l) {
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
      else return oDocRs_mk_some(docRs_new(buf_to_str(bf), l));
    }

    if (str_starts(ltrim, "/**")) {
      char *l = file_read_line(f);
      char *ltrim = str_ltrim(l);
      if (!*ltrim || str_index(ltrim, "*/") != -1)
        return oDocRs_mk_none();
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
      else return oDocRs_mk_some(docRs_new(buf_to_str(bf), l));
    }

    return oDocRs_mk_none();
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

static Doc *module_read (char *path) {
  FileLck *f = file_ropen(path);
  int is_first = 1;
  Doc *doc = doc_new("");

  for (;;) {
    char *l = file_read_line(f);
    if (*str_ltrim(l) == '/') {
      DocRs *rs = oDocRs_nsome(read_doc(f, l));
      if (rs && *rs->line) {
        char *cd = read_code(f, rs->line, is_first);
        char *cdtrim = str_trim(cd);
        is_first = 0;
        if (str_eq(cd, "overview")) {
          doc = doc_new(rs->doc);
        } else if (str_starts(cd, "#define ")) {
          aDocEntry_push(doc->defines, docEntry_new(
            utx_read_name(cdtrim, strlen("#define ")),
            rs->doc, cd, utx_mk_link(cdtrim)
          ));
        } else if (str_starts(cd, "typedef ")) {
          int ix = str_cindex(cdtrim, ')');
          if (ix == -1) ix = strlen(cdtrim);
          aDocEntry_push(doc->typedefs, docEntry_new(
            utx_read_namebk(cdtrim, ix),
            rs->doc, cd, utx_mk_link(cdtrim)
          ));
        } else if (str_cindex(cdtrim, '{') != -1) {
          if (str_starts(cd, "enum ")) {
            aDocEntry_push(doc->enums, docEntry_new(
              utx_read_name(cdtrim, strlen("enum ")),
              rs->doc, cd, utx_mk_link(cdtrim)
            ));
          }
          if (str_starts(cd, "struct ")) {
            aDocEntry_push(doc->structs, docEntry_new(
              utx_read_name(cdtrim, strlen("struct ")),
              rs->doc, cd, utx_mk_link(cdtrim)
            ));
          }
          if (str_starts(cd, "union ")) {
            aDocEntry_push(doc->unions, docEntry_new(
              utx_read_name(cdtrim, strlen("union ")),
              rs->doc, cd, utx_mk_link(cdtrim)
            ));
          }
        } else {
          int ix = str_cindex(cdtrim, '=');
          if (ix != -1) {
            aDocEntry_push(doc->vars, docEntry_new(
              utx_read_namebk(cdtrim, ix),
              rs->doc, cd, utx_mk_link(cdtrim)
            ));
          } else {
            ix = str_cindex(cdtrim, '(');
            if (ix != -1) {
              if (str_starts(str_trim(str_right(cdtrim, ix + 1)), "*"))
                ix = str_cindex_from(cdtrim, ix, ')');
              aDocEntry_push(doc->functions, docEntry_new(
                utx_read_namebk(cdtrim, ix),
                rs->doc, cd, utx_mk_link(cdtrim)
              ));
            } else if (*cdtrim) {
              aDocEntry_push(doc->vars, docEntry_new(
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

ODoc *moduleRd_read (char *lib, char *path) {
  /**/int find_lib(Dpath *p) { return str_eq(p->id, lib); }
  Dpath *lb = oDpath_nsome(aDpath_find(dpaths_read(), find_lib));

  if (lb) {
    char *ipath = path_cat(lb->path, "include", str_f("%s.h", path), NULL);
    if (file_exists(ipath)) {
      return oDoc_mk_some(module_read(ipath));
    }
  }
  return oDoc_mk_none();
}
