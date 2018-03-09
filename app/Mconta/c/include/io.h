// Copyright 06-Mar-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// File system management

#ifndef IO_H
  #define IO_H

#include "dmc/all.h"

///
void io_init(Cgi *cgi, char *app_name, char *data_version);

///
Json *io_read_conf();

///
void io_write_conf(Json *data);

#endif
