// Copyright 11-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#ifndef DATA_ISSUE_H
  #define DATA_ISSUE_H

#include "dmc/std.h"
#include "Quote.h"

///
enum Issue_t {
  ISSUE_NONE, // There is no issue
  ISSUE_EXISTS, // Nick_id does not exists
  ISSUE_SERVER, // Code of server is missing
  ISSUE_EMPTY, // Not data at all
  ISSUE_MISSING, // Missing quote
  ISSUE_EXTRA, // Extra quote
  ISSUE_BEFORE_NOW, // Current quote varies +- 20%
  ISSUE_MAX, // Open or Close > Max
  ISSUE_MIN // Open or Close < Min
  };

/*.-.*/

#include "dmc/Json.h"

///
typedef struct issue_Issue Issue;

///
Issue *issue_new(char *id, enum Issue_t type, char *cause);

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
Issue *issue_check_quotes(char *nick_id, Aquote *qs);

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

/// Passes 'qs' to char * inserting a error mark.
char *issue_annotate(Aquote *qs, Issue *i);

#endif
