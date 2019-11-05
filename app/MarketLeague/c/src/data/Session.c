// Copyright 27-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Session.h"

/* .
# Session data.
Session: serial
  # Arr<char> First division nicks.
  nicks1: Arr - char *
  # First division league
  l1: MatchLeague

  # Arr<char> Second division nicks.
  nicks2: Arr - char *
  # Second division league
  l2: MatchLeague

  # Arr<char> Third division nicks.
  nicks3: Arr - char *
  # Third division league
  l3: MatchLeague
*/
/*--*/

struct Session_Session {
  Arr *nicks1;
  MatchLeague *l1;
  Arr *nicks2;
  MatchLeague *l2;
  Arr *nicks3;
  MatchLeague *l3;
};

Session *session_new (
  Arr *nicks1,
  MatchLeague *l1,
  Arr *nicks2,
  MatchLeague *l2,
  Arr *nicks3,
  MatchLeague *l3
) {
  Session *this = MALLOC(Session);
  this->nicks1 = nicks1;
  this->l1 = l1;
  this->nicks2 = nicks2;
  this->l2 = l2;
  this->nicks3 = nicks3;
  this->l3 = l3;
  return this;
}

Arr *session_nicks1 (Session *this) {
  return this->nicks1;
}

MatchLeague *session_l1 (Session *this) {
  return this->l1;
}

Arr *session_nicks2 (Session *this) {
  return this->nicks2;
}

MatchLeague *session_l2 (Session *this) {
  return this->l2;
}

Arr *session_nicks3 (Session *this) {
  return this->nicks3;
}

MatchLeague *session_l3 (Session *this) {
  return this->l3;
}

Js *session_to_js (Session *this) {
  // Arr[Js]
  Arr *js = arr_new();
  arr_push(js, arr_to_js(this->nicks1, (FTO)js_ws));
  arr_push(js, matchLeague_to_js(this->l1));
  arr_push(js, arr_to_js(this->nicks2, (FTO)js_ws));
  arr_push(js, matchLeague_to_js(this->l2));
  arr_push(js, arr_to_js(this->nicks3, (FTO)js_ws));
  arr_push(js, matchLeague_to_js(this->l3));
  return js_wa(js);
}

Session *session_from_js (Js *js) {
  // Arr[Js]
  Arr *a = js_ra(js);
  Js **p = (Js **)arr_start(a);
  Session *this = MALLOC(Session);
  this->nicks1 = arr_from_js(*p++, (FFROM)js_rs);
  this->l1 = matchLeague_from_js(*p++);
  this->nicks2 = arr_from_js(*p++, (FFROM)js_rs);
  this->l2 = matchLeague_from_js(*p++);
  this->nicks3 = arr_from_js(*p++, (FFROM)js_rs);
  this->l3 = matchLeague_from_js(*p++);
  return this;
}

/*--*/
