// Copyright 04-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Configuration data base.<br>
/// It structure is in <a href='#hp:conf_read'>conf_read</a>.

#ifndef IO_CONF_H
  #define IO_CONF_H

#include "dmc/std.h"

///
void conf_init (void);

/// 'lang' can be "es" or "en"
char *conf_lang (void);

/// 'lang' can be "es" or "en"
void conf_set_lang (char *lang);

char *conf_sys_page (void);

/// 'sys_page' Not initialized. Values from javascript.
void conf_set_sys_page (char *sys_page);

/// Returns id of selected nick in sys_page->nicks or -1 if no one is selected.
int conf_nick_sel_id (void);

/// Sets id of selected nick in sys_pate->nicks.
void conf_set_nick_sel_id (int id);

#endif
