// Copyright 07-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#ifndef DATA_MDOC_H
  #define DATA_MDOC_H

#include "dmc/std.h"
#include "Class.h"

/*.-.*/

#include "dmc/Json.h"

///
typedef struct mdoc_Mdoc Mdoc;

///
Mdoc *mdoc_new(char *doc, ApackDoc *functions, Aclass *classes);

///
char *mdoc_doc(Mdoc *this);

///
ApackDoc *mdoc_functions(Mdoc *this);

///
Aclass *mdoc_classes(Mdoc *this);

///
Json *mdoc_to_json(Mdoc *this);

///
Mdoc *mdoc_from_json(Json *s);

/*.-.*/

#endif

