// Copyright 11-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Issue.h"
#include "data/nicks_db.h"
#include "data/servers_db.h"
#include "data/Server.h"
#include "dmc/ct/Ochar.h"

/* .+.
struct: @Issue
  id: char *: _string
  type: enum Issue_t: _int
  cause: char *: _string
*/

/*.-.*/
#include "dmc/ct/Ajson.h"

struct issue_Issue {
  char *id;
  enum Issue_t type;
  char *cause;
};

Issue *_issue_new(char *id, enum Issue_t type, char *cause) {
  Issue *this = MALLOC(Issue);
  XNULL(id)
  this->id = id;
  this->type = type;
  XNULL(cause)
  this->cause = cause;
  return this;
}

char *issue_id(Issue *this) {
  XNULL(this)
  return this->id;
}

enum Issue_t issue_type(Issue *this) {
  XNULL(this)
  return this->type;
}

char *issue_cause(Issue *this) {
  XNULL(this)
  return this->cause;
}

Json *issue_to_json(Issue *this) {
  XNULL(this)
  Ajson *serial = ajson_new();
  jarr_astring(serial, this->id);
  jarr_aint(serial, this->type);
  jarr_astring(serial, this->cause);
  return json_warray(serial);
}

Issue *issue_from_json(Json *js) {
  XNULL(js)
  Ajson *serial = json_rarray(js);
  Issue *this = MALLOC(Issue);
  size_t i = 0;
  this->id = jarr_gstring(serial, i++);
  this->type = jarr_gint(serial, i++);
  this->cause = jarr_gstring(serial, i++);
  return this;
}
/*.-.*/

static Issue *check(char *nick_id, char *nick_name) {
  // --------------------------------------------------- ------ SERVER
  char *msg = "";
  EACH(servers_db_list(), Server, s) {
    bool missing = true;
    EACHI(mchar_keys(server_nicks(s)), char, id) {
      if (str_eq(id, nick_id)) {
        missing = false;
        break;
      }
    }_EACH
    if (missing) {
      msg = str_printf("Server code of %s is missing", server_name(s));
      break;
    }
  }_EACH
  if (*msg) {
    return _issue_new(nick_id, ISSUE_SERVER, msg);
  }

  // ------------------------------------------------------------ NONE
  return _issue_new(nick_id, ISSUE_NONE, "");
}

Issue *issue_check(char *nick_id) {
  // ---------------------------------------------------------- EXISTS
  Ochar *onick_name = nicks_db_name(nick_id);
  if (ochar_is_null(onick_name)) {
    return _issue_new(
      nick_id, ISSUE_EXISTS,
      str_printf("Nick id '%s' does not exist", nick_id)
    );
  }
  return check(nick_id, ochar_value(onick_name));
}

#define TY Issue
#define FN issue
#include "dmc/tpl/topt.c"
#include "dmc/tpl/tarr.c"
#undef TY
#undef FN

Oissue *issue_search(void) {
  EACH(nicks_db_list(), Nick, n) {
    Issue *i = check(nick_id(n), nick_name(n));
    if (i->type != ISSUE_NONE) {
      return oissue_new(i);
    }
  }_EACH
  return oissue_null();
}
