// Copyright 06-Mar-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Configuration management

#ifndef CONF_H
  #define CONF_H

#include "dmc/all.h"

/*.-.*/

///
typedef struct conf_Conf Conf;

///
char *conf_lang(Conf *this);

///
void conf_set_lang(Conf *this, char *value);

///
char *conf_pg(Conf *this);

///
void conf_set_pg(Conf *this, char *value);

///
char *conf_pg2(Conf *this);

///
void conf_set_pg2(Conf *this, char *value);

/*.-.*/

///
Conf *conf_read();

///
void conf_write(Conf *this);

#endif
