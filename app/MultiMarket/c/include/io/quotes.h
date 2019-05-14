// Copyright 10-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#ifndef IO_QUOTES_H
  #define IO_QUOTES_H

#include "dmc/async.h"
#include "data/Quote.h"

///
#define quotes_OK "QuotesOk"
#define quotes_IO "QuotesIo"
#define quotes_EMPTY "QuotesEmpty"
#define quotes_MISSING "QuotesMissing"
#define quotes_SYNTAX "QuotesSyntax"

///
void quotes_init (void);

/// Returns quotes of 'nick_name'.
///   nick_name: Name of nick (e.g. TEF)
///   return: Kv[Arr[Quote]]. Key can be: quotes_OK, quotes_IO, quotes_EMPTY,
///                                      quotes_MISSING ot quotes_SYNTAX
///           If an error results, value is an empty array.
///           Order of quotes is from after to before
Kv *quotes_read (char *nick_name);

/// Writes 'qs' of 'nick_name'
///   nick_name: Name of nick (e.g. TEF)
///   qs: It is Arr[Quote]. Its order is from after to before.
///   return: It can be quotes_OK, quotes_EMPTY or quotes_MISSING
char *quotes_write (char *nick_name, Arr *qs);

/// Creates a new empty file if it do not already exist.
///   nick_name: Name of nick (e.g. TEF)
void quotes_add (char *nick_name);

/// Deletes a file if it exist.
///   nick_name: Name of nick (e.g. TEF)
void quotes_del (char *nick_name);

/// Changes file 'old_name' by 'new_name' if 'old_name' exists
void quotes_modify (char *old_name, char *new_name);

#endif
