// Copyright 06-Mar-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "Conf.h"
#include "io.h"

/*.+.
-struct: @Conf
  +lang: char *
  +pg: char *
  +pg2: char *
*/

/*.-.*/
struct conf_Conf {
  char *lang;
  char *pg;
  char *pg2;
};

Conf *_conf_new(char *lang, char *pg, char *pg2) {
  Conf *this = MALLOC(Conf);
  this->lang = lang;
  this->pg = pg;
  this->pg2 = pg2;
  return this;
}

inline
char *conf_lang(Conf *this) {
  return this->lang;
}

inline
void conf_set_lang(Conf *this, char *value) {
  this->lang = value;
}

inline
char *conf_pg(Conf *this) {
  return this->pg;
}

inline
void conf_set_pg(Conf *this, char *value) {
  this->pg = value;
}

inline
char *conf_pg2(Conf *this) {
  return this->pg2;
}

inline
void conf_set_pg2(Conf *this, char *value) {
  this->pg2 = value;
}
/*.-.*/

Conf *conf_read() {
  Map/*Json*/ *m = json_robject(io_read_conf());
  char *lang = map_get(m, "lang");
  char *pg = map_get(m, "pg");
  char *pg2 = map_get(m, "pg2");

  return _conf_new(
    lang ? json_rstring(lang) : "es",
    pg ? json_rstring(pg) : "portfolio",
    pg2 ? json_rstring(pg2) : "view"
  );
}

inline
void conf_write(Conf *this) {
  Map/*Json*/ *m = map_new();
  jmap_pstring(m, "lang", this->lang);
  jmap_pstring(m, "pg", this->pg);
  jmap_pstring(m, "pg2", this->pg2);
  io_write_conf(json_wobject(m));
}
