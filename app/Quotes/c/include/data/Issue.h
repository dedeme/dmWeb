// Copyright 11-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#ifndef DATA_ISSUE_H
  #define DATA_ISSUE_H

#include "dmc/std.h"

enum Issue_t {
  ISSUE_NONE, // There is no issue
  ISSUE_EXISTS, // Nick_id does not exists
  ISSUE_SERVER, // Code of server is missing
  ISSUE_EMPTY, // Not data at all
  ISSUE_MISSING, // Missing data of a date
  ISSUE_FIELD_MISSING, // Missing value in a field
  ISSUE_BEFORE_NOW, // Current quote varies +- 20%
  ISSUE_MAX, // Open or Close > Max
  ISSUE_MIN // Open or Close < Min
  };

/*.-.*/

#include "dmc/Json.h"

///
typedef struct issue_Issue Issue;

///
char *issue_id(Issue *this);

///
enum Issue_t issue_type(Issue *this);

///
char *issue_cause(Issue *this);

///
Json *issue_to_json(Issue *this);

///
Issue *issue_from_json(Json *s);

/*.-.*/

///
Issue *issue_check(char *nick_id);

#define TY Issue
#define FN issue
#include "dmc/tpl/topt.h"
#include "dmc/tpl/tarr.h"
#undef TY
#undef FN

///
Oissue *issue_search(void);

#endif
