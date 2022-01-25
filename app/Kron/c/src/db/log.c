// Copyright 13-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "db/log.h"
#include "dmc/path.h"
#include "dmc/file.h"
#include "data/LogEntry/ALogEntry.h"

static char *log_path = NULL;

void log_init (char *parent) {
  log_path = path_cat(parent, "log.tb", NULL);
  if (!file_exists(log_path)) {
    log_write(aLogEntry_new());
  }
}

char *log_read_js (void) {
  return file_read(log_path);
}

ALogEntry *log_read (void) {
  return aLogEntry_from_js(log_read_js(), logEntry_from_js);
}

void log_write (ALogEntry *log) {
  log = aLogEntry_take(log, 500);
  file_write(log_path, aLogEntry_to_js(log, logEntry_to_js));
}

void log_info (char *msg) {
  ALogEntry *log = log_read();
  aLogEntry_push(log, logEntry_new("I", msg));
  log_write(log);
}

void log_msg (char *msg) {
  ALogEntry *log = log_read();
  aLogEntry_push(log, logEntry_new("M", msg));
  log_write(log);
}
