// Copyright 10-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Server configuration (for daily and historic configurations)

#ifndef DATA_RCONF_H
  #define DATA_RCONF_H

#include "dmc/async.h"

/*--*/

///
///   Arguments:
///     cmd: char*
///     url: char*
///     regex: char*
///     sel: int
///     is_date_eu: bool
///     date_separator: char*
///     is_iso_number: bool
///     fields_type: char*
///     table_start: char*
///     table_end: char*
///     row_start: char*
///     row_end: char*
///     cols_start: Arr-char*
///     cols_end: Arr-char*
typedef struct Rconf_Rconf Rconf;

///
char *rconf_cmd (Rconf *this);

///
char *rconf_url (Rconf *this);

///
char *rconf_regex (Rconf *this);

/// enum Server
int rconf_sel (Rconf *this);

///
void rconf_set_sel (Rconf *this, int value);

///
int rconf_is_date_eu (Rconf *this);

///
char *rconf_date_separator (Rconf *this);

///
int rconf_is_iso_number (Rconf *this);

///
char *rconf_fields_type (Rconf *this);

///
char *rconf_table_start (Rconf *this);

///
char *rconf_table_end (Rconf *this);

///
char *rconf_row_start (Rconf *this);

///
char *rconf_row_end (Rconf *this);

///
Arr *rconf_cols_start (Rconf *this);

///
Arr *rconf_cols_end (Rconf *this);

///
Js *rconf_to_js (Rconf *this);

///
Rconf *rconf_from_js (Js *js);

/*--*/

#endif
