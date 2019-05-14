// Copyright 03-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#ifndef IO_H
  #define IO_H

#include "dmc/async.h"

/// Initializes 'io'. It must be called at the beginning of application.
void io_init (void);

/// Finalizes 'io'. It must be called at the end of application.
void io_end (void);

/// active value is set to '1' in io_init and must be set '0' to end the
///  application.
void io_set_active (int value);

/// returns if application is active.
int io_active (void);

/// Returns 'data' directory
char *io_data_dir (void);

/// Returns 'tmp' directory
char *io_tmp_dir (void);

/// Clears 'tmp' dir.
void io_clear_tmp (void);

/// Returns millisecons since epoch
char *io_time_stamp (void);

/// Execute 'fn(value)' asyncronically. This function returns immediatly.
///   NOTE: Neither 'value' nor 'fn' can be a local value.
void io_run (void (*fn)(void *), void *value);

/// Execute 'fn(value)' asyncronically. This function returns after finalizing
/// 'fn'.
void io_wait (void (*fn)(void *), void *value);

#endif
