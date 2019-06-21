// Copyright 04-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Management of 'conf&#46;db'

#ifndef IO_CONF_H
  #define IO_CONF_H

#include "dmc/std.h"

///
void conf_init (void);

/// 'lang' can be "es" or "en"
char *conf_lang (void);

/// 'lang' can be "es" or "en"
void conf_set_lang (char *lang);

///
char *conf_sys_page (void);

///
void conf_set_sys_page (char *sys_page);

/// Returns id of selected nick in sys_page->nicks or -1 if no one is selected.
int conf_nick_sel_id (void);

/// Sets id of selected nick in sys_pate->nicks.
void conf_set_nick_sel_id (int id);

/// Returns id of selected server in sys_page->servers or -1 if no one is
/// selected.
int conf_server_sel_id (void);

/// Sets id of selected server in sys_page->servers.
void conf_set_server_sel_id (int id);

/// Returns name of selected tab in sys_page->servers or "" if no one is
/// selected.
char *conf_server_tab (void);

/// Set name of selected tab in sys_page->servers.
void conf_set_server_tab (char *tab_name);

///
char *conf_fleas_page (void);

///
void conf_set_fleas_page (char *fleas_page);

/// Returns model for fleas->bests and fleas-charts
char *conf_fleas_model (void);

/// Sets model for fleas->bests and fleas-charts
void conf_set_fleas_model (char *model);

/// Returns identifier of activity. It can be ACT_SLEEPING1, ACT_SLEEPING2,
/// ACT_ACTIVATING, ACT_ACTIVE or ACT_DEACTIVATING.
char *conf_activity (void);

/// Sets identifier of activity. It can be ACT_SLEEPING1, ACT_SLEEPING2,
/// ACT_ACTIVATING, ACT_ACTIVE or ACT_DEACTIVATING.
void conf_set_activity (char *activity_id);

/// Returns if fleas are running.
int conf_fleas_running (void);

/// Sets if fleas are running.
void conf_set_fleas_running (int value);

#endif
