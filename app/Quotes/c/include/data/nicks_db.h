// Copyright 11-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#ifndef DATA_NICK_DB_H
  #define DATA_NICK_DB_H

#include "dmc/std.h"
#include "Nick.h"
#include "Quote.h"

/*.-.*/

#include "dmc/Json.h"

///
typedef struct nick_db_Nick_db Nick_db;

///
Json *nick_db_to_json(Nick_db *this);

///
Nick_db *nick_db_from_json(Json *s);

/*.-.*/

/// Returns the nick model id.
char *nicks_db_model(void);

/// Returns the nicks list.
Anick *nicks_db_list(void);

/// Returns the nick name of a nick 'id'.
Ochar *nicks_db_name(char *id);

/// Returns the nick name of model.
char *nicks_db_model_name(void);

/// Adds a nick and returns false if action fails because 'name'
/// is duplicated.
bool nicks_db_add(char *name, bool is_ibex, bool is_sel);

/// Removes the nick with identifier 'id'
void nicks_db_remove(char *id);

/// Modifies the value of 'is_ibex' of a nick with identifier 'id'
void nicks_db_set_ibex(char *id, bool is_ibex);

/// Modifies the value of 'is_sel' (it is included en the selection list) of a
/// nick with identifier 'id'
void nicks_db_set_sel(char *id, bool is_sel);

/// Modifies the name of a nick and returns 'false' if the action fails
/// because 'name' is duplicated.
bool nicks_db_set_name(char *id, char *name);

/// Sets as model the nick with identifier 'id'
void nicks_db_set_model(char *id);

/// If fails returns ""
char *nicks_db_quotes_str(char *id);

///
Oaquote *nicks_db_quotes(char *id);

///
void nicks_db_set_quotes(char *id, Aquote *qs);

///
Oaquote *nicks_db_model_quotes(void);

#endif
