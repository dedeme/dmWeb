// Copyright 12-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Global constants.

#ifndef DATA_CTS_H
  #define DATA_CTS_H

#define SFAIL(msg) str_f("%s:%d:[%s]:\n%s", __FILE__, __LINE__, __FUNCTION__, (msg))

/// Application name.
char *cts_app_name (void);

/// Data directory relative to sys_home()
char *cts_data_dir (void);

///
char *cts_data_version (void);

/// Communications port.
char *cts_port (void);

/// Expiration time.
int cts_expiration (void);

/// Time to scheduler sleep (milliseconds).
int cts_scheduler_sleep (void);

/// Cycles of 'scheduler sleep' to test tasks.
int cts_scheduler_times (void);

/// Time to server sleep (milliseconds).
int cts_server_sleep (void);

/// Returns the ok message, when server is running.
char *cts_server_ok_msg (void);

/// Returns the message to stop server.
char *cts_server_stop_msg (void);

#endif
