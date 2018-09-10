// Copyright 06-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#ifndef DB_H
  #define DB_H

#include "dmc/std.h"
#include "dmc/ct/Mjson.h"

///
void db_init(void);

/// Returns conf data:
///   jmap_pstring(m, "path", path);
///   jmap_pstring(m, "lang", lang);
///   jmap_pbool(m, "show", show);
Mjson *db_conf(void);

///
void db_set_lang(char *lang);

///
void db_set_path(char *path);

///
void db_set_show_all(bool value);

/// Returns an AmenuPath jsonized.
Json *db_paths(void);

///
void db_add_path(char *id, char *path);

///
void db_set_show(char *id, bool value);

///
void db_delete(char *id);

///
void db_modify(char *old_id, char *new_id, char *path);

#endif
