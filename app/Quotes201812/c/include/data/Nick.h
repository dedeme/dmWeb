// Copyright 11-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#ifndef DATA_NICK_H
  #define DATA_NICK_H

#include "dmc/std.h"

/*.-.*/

#include "dmc/Json.h"

///
typedef struct nick_Nick Nick;

///
Nick *nick_new(
  char *id,
  char *name,
  bool is_ibex,
  bool is_sel
);

///
char *nick_id(Nick *this);

///
char *nick_name(Nick *this);

///
void nick_set_name(Nick *this, char *value);

///
bool nick_is_ibex(Nick *this);

///
void nick_set_is_ibex(Nick *this, bool value);

///
bool nick_is_sel(Nick *this);

///
void nick_set_is_sel(Nick *this, bool value);

///
Json *nick_to_json(Nick *this);

///
Nick *nick_from_json(Json *s);

/*.-.*/

#define TY Nick
#define FN nick
#include "dmc/tpl/tarr.h"
#undef TY
#undef FN

///
Json *anick_to_json(Anick *this);

///
Anick *anick_from_json(Json *js);

#endif
