// Copyright 10-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#ifndef DATA_RCONF_H
  #define DATA_RCONF_H

#include "dmc/async.h"

/*--*/

///
typedef struct Rconf_Rconf Rconf;

///
char *rconf_url(Rconf *this);

///
int rconf_sel(Rconf *this);

///
void rconf_set_sel(Rconf *this, int value);

///
int rconf_is_date_eu(Rconf *this);

///
char *rconf_date_separator(Rconf *this);

///
int rconf_is_iso_number(Rconf *this);

///
char *rconf_fields_type(Rconf *this);

///
char *rconf_table_start(Rconf *this);

///
char *rconf_table_end(Rconf *this);

///
char *rconf_row_start(Rconf *this);

///
char *rconf_row_end(Rconf *this);

///
Arr *rconf_cols_start(Rconf *this);

///
Arr *rconf_cols_end(Rconf *this);

///
Js *rconf_to_js(Rconf *this);

///
Rconf *rconf_from_js(Js *js);

/*--*/

#endif
