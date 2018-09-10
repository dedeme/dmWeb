// Copyright 07-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#ifndef DATA_CLASS_H
  #define DATA_CLASS_H

#include "dmc/std.h"
#include "PackDoc.h"
#include "ClassAtt.h"

/// Struct which represents a class.
/// It has the folowing fields:
///   id: Identifier of class
///   definition: Definition o class
///   constructor: Data of constructor. It there is no contructor all its
///                values are blanks ("").
///   methods: Arr[PackDoc]. Methods data.
///   atts: Arr[ClassAtt]. Attrubutes data.

/*.-.*/

#include "dmc/Json.h"

///
typedef struct class_Class Class;

///
Class *class_new(
  char *id,
  char *definition,
  char *doc,
  PackDoc *constructor,
  ApackDoc *methods,
  AclassAtt *atts
);

///
char *class_id(Class *this);

///
char *class_definition(Class *this);

///
char *class_doc(Class *this);

///
PackDoc *class_constructor(Class *this);

///
ApackDoc *class_methods(Class *this);

///
AclassAtt *class_atts(Class *this);

///
Json *class_to_json(Class *this);

///
Class *class_from_json(Json *s);

/*.-.*/

#define TY Class
#define FN class
#include "dmc/tpl/tarr.h"
#undef TY
#undef FN


#endif
