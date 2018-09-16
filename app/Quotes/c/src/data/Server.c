// Copyright 10-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Server.h"
#include "data/db.h"

/* .+.
-struct: Server
  name: char *
  read: fread_t
  read_last: fread_last_t
*/

/*.-.*/
#include "dmc/ct/Ajson.h"

struct server_Server {
  char *name;
  fread_t read;
  fread_last_t read_last;
};

Server *server_new(char *name, fread_t read, fread_last_t read_last) {
  Server *this = MALLOC(Server);
  XNULL(name)
  this->name = name;
  this->read = read;
  this->read_last = read_last;
  return this;
}

char *server_name(Server *this) {
  XNULL(this)
  return this->name;
}

fread_t server_read(Server *this) {
  XNULL(this)
  return this->read;
}

fread_last_t server_read_last(Server *this) {
  XNULL(this)
  return this->read_last;
}
/*.-.*/

Mchar *server_nicks(Server *this) {
  char *path = path_cat(
    db_dir(),
    str_cat("server", this->name, NULL),
    "codes.db",
    NULL
  );
  if (file_exists(path)) {
/*
    Json *js = (Json *)file_read(path);
    Ajson *a = json_rarray(js);
    if (ajson_size(a) > 2) {
      Mchar *r = mchar_new();
      EACH(a, Json, js2) {
        Ajson *a2 = json_rarray(js2);
        mchar_put(r,
          json_rstring(ajson_get(a2, 0)), json_rstring(ajson_get(a2, 1)));
      }_EACH
      return r;
    } else {
      return mchar_from_json(js, json_rstring);
    }
*/
    return mchar_from_json((Json *)file_read(path));
  }
  return mchar_new();
}

/// 'codes' es a map QuotesId -> ServerId
void server_set_nicks(Server *this, Mchar *codes) {
  char *dir = path_cat(db_dir(), str_cat("server", this->name, NULL), NULL);
  char *path = path_cat(dir, "codes.db", NULL);

  if (!file_exists(dir)) {
    file_mkdir(dir);
  }

  file_write(path, (char *)mchar_to_json(codes));
}

#define TY Server
#define FN server
#include "dmc/tpl/tarr.c"
#undef TY
#undef FN
