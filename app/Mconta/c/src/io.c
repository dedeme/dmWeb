// Copyright 06-Mar-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "io.h"

static char *BOLSA_EXTERN = "/deme/wwwcgi/dmcgi/BolsaData/data";
static char *DATA_VERSION = "BolsaData\nData version: 201711";

static char *bolsa = NULL;
static char *bolsa_data = NULL;

static char *data = NULL;
static char *data_version = NULL;
static char *data_conf = NULL;

static char *tmp = NULL;
static char *backups = NULL;
static char *trash = NULL;

static void control_bolsa_data () {
  if (!file_exists(BOLSA_EXTERN)) {
    THROW "Directory '%s' not found", BOLSA_EXTERN _THROW
  }
  char *version = path_cat(BOLSA_EXTERN, "version.txt", NULL);
  if (!file_exists(version)) {
    THROW "Directory '%s' does not contains 'version.txt'", BOLSA_EXTERN _THROW
  }
  if (!str_starts(file_read(version), DATA_VERSION)) {
    THROW
      "Directory '%s' version is:\n%s but should be:\n%s  ",
      BOLSA_EXTERN, file_read(version), DATA_VERSION
    _THROW
  }
}

void io_init(Cgi *cgi, char *app_name, char* dversion) {
  bolsa = path_cat(cgi_home(cgi), "bolsa_data", NULL);
  bolsa_data = path_cat(bolsa, "data.db", NULL);

  data = path_cat(cgi_home(cgi), "data", NULL);
  data_version = path_cat(data, "version.txt", NULL);
  data_conf = path_cat(data, "conf.db", NULL);

  tmp = path_cat(cgi_home(cgi), "tmp", NULL);

  backups = path_cat(cgi_home(cgi), "backups", NULL);

  trash = path_cat(cgi_home(cgi), "trash", NULL);


  if (!file_exists(bolsa_data)) {
    control_bolsa_data();
    file_link(BOLSA_EXTERN, bolsa);
  }

  if (!file_exists(data)) {
    file_mkdir(data);
  }
  if (!file_exists(data_version)) {
    file_write(
      data_version,
      str_printf("%s\nData version: %s\n", app_name, dversion)
    );
  }
  if (!file_exists(data_conf)) {
    file_write(data_conf, json_wobject(map_new()));
  }

  if (!file_exists(tmp)) {
    file_mkdir(tmp);
  }

  if (!file_exists(backups)) {
    file_mkdir(backups);
  }

  if (!file_exists(trash)) {
    file_mkdir(trash);
  }


}

Json *io_read_conf() {
  return file_read(data_conf);
}

void io_write_conf(Json *jdata) {
  file_write(data_conf, jdata);
}

