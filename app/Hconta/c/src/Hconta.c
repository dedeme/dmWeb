// Copyright 16-Feb-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include <stdlib.h>
#include <stdio.h>
#include <gc.h>
#include "dmc/Date.h"
#include "dmc/str.h"
#include "dmc/Cgi.h"
#include "dmc/path.h"
#include "dmc/file.h"
#include "dmc/Json.h"
#include "dmc/ext.h"
#include "dmc/exc.h"
#include "dmc/b64.h"
#include "dmc/cryp.h"
#include "dmc/DEFS.h"
#include "dmc/ct/Achar.h"
#include "dmc/ct/Ajson.h"
#include "dmc/ct/Mjson.h"
#include "Hconta.h"

static char *app_name = "Hconta";
static char *data_version = "201709";
static char *app_dir = "dmcgi/Hconta";
static time_t expiration = 1800;  // 1/2 hour

static char *mk_date() {
  return date_format(date_now(), "%Y%m%d");
}

static char *mk_date2() {
	Date t = date_now();
  return str_printf("%s-%ld", date_format(t, "%Y%m%d"), t);
}

static void hconta_init(Cgi *cgi) {
	char *dir = path_cat(app_dir, "data", NULL);

  if (!file_exists(dir)) {
    file_mkdir(dir);
    char *version = str_printf(
      "%s\nData version: %s\n", app_name, data_version
    );
    char *fversion = path_cat(dir, "version.txt", NULL);
    file_write(fversion, version);
    file_mkdir(path_cat(app_dir, "tmp", NULL));
    file_mkdir(path_cat(app_dir, "backups", NULL));
    file_mkdir(path_cat(app_dir, "trash", NULL));
  }
}

static Ajson *read_backups(Cgi *cgi) {
  Achar *fs = file_dir(path_cat(app_dir, "backups", NULL));

  Ajson *rs = ajson_new();
  EACH(fs, char, f) {
    ajson_add(rs, json_wstring(path_name(f)));
  }_EACH
	return rs;
}

static void filter_backups(Cgi *cgi) {
  Date t0 = date_now();
  Date t1 = date_add(t0, -7);
  Date t2 = date_new(date_day(t0), date_month(t0), date_year(t0) - 1);

  char *d1 = date_format(t1, "%Y%m%d");
  char *d2 = date_format(t2, "%Y%m%d");

  Achar *fs = file_dir(path_cat(app_dir, "backups", NULL));
  achar_sort(fs);
  char *previous = "        ";
  EACH(fs, char, f) {
    char *name = path_only_name(f);
    if (str_cmp(name, d2) < 0) {
      char *cut1 = str_sub(name, 0, 4);
      char *cut2 = str_sub(previous, 0, 4);
      if (str_eq(cut1, cut2)) {
        file_del(f);
      }
    } else if (str_cmp(name, d1) < 0) {
      char *cut1 = str_sub(name, 0, 6);
      char *cut2 = str_sub(previous, 0, 6);
      if (str_eq(cut1, cut2)) {
        file_del(f);
      }
    }
    previous = name;
  }_EACH
}

static Ajson *read_trash(Cgi *cgi) {
  Achar *fs = file_dir(path_cat(app_dir, "trash", NULL));

  Ajson *rs = ajson_new();
  EACH(fs, char, f) {
    ajson_add(rs, json_wstring(path_name(f)));
  }_EACH

	return rs;
}

static void to_trash(Cgi *cgi) {
  ext_zip(
    path_cat(app_dir, "data", NULL),
    path_cat(app_dir, "trash", str_printf("%s.zip", mk_date2()), NULL)
  );
}

static void clear_tmp(Cgi *cgi) {
  char *dir = path_cat(app_dir, "tmp", NULL);
  file_del(dir);
  file_mkdir(dir);
}

char *unzip(Cgi *cgi) {
  char *fail = "";
  TRY {
    ext_unzip(
      path_cat(app_dir, "tmp", "back.zip", NULL),
      path_cat(app_dir, "tmp", NULL)
    );
  } CATCH(e) {
    fail = "restore:unzip";
  }_TRY
  if (*fail) {
    return fail;
  }

  char *source = path_cat(app_dir, "tmp", "data", "version.txt", NULL);
  if (!file_exists(source)) {
    return "restore:version does not exist";
  } else {
    char *good_version = str_printf(
      "%s\nData version: %s\n", app_name, data_version
    );
    char *version = file_read(source);
    if (str_cmp(version, good_version)) {
      return "restore:version is wrong";
    } else {
      return "";
    }
  }
}

static CgiRp *app_process(Cgi *cgi, char *session_id, Mjson *rqm) {
  char *home = app_dir;
  char *rq = jmap_gstring(rqm, "rq");
  // ------------------------------------------------------- getConf
  if (str_eq(rq, "getConf")) {
    char *path = path_cat(home, "data", "conf.db", NULL);
    char *data = "";
    if (file_exists(path)) {
      data = file_read(path);
    }

    Mjson *m = mjson_new();
    jmap_pstring(m, "conf", data);
    return cgi_ok(cgi, m);

  // ------------------------------------------------------- setConf
  } else if (str_eq(rq, "setConf")) {
    file_write(
      path_cat(home, "data", "conf.db", NULL),
      jmap_gstring(rqm, "conf")
    );

    Mjson *m = mjson_new();
    return cgi_ok(cgi, m);

  // ------------------------------------------------------- getDb
  } else if (str_eq(rq, "getDb")) {
    char *path = path_cat(
      home, "data", str_printf("%s.db", jmap_gstring(rqm, "year")), NULL
    );
    char *data = "";
    if (file_exists(path)) {
      data = file_read(path);
    }

    Mjson *m = mjson_new();
    mjson_put(m, "db", json_wstring(data));
    mjson_put(m, "backups", json_warray(read_backups(cgi)));
    mjson_put(m, "trash", json_warray(read_trash(cgi)));
    return cgi_ok(cgi, m);

  // ------------------------------------------------------- setDb
  } else if (str_eq(rq, "setDb")) {
    file_write(
      path_cat(
        home, "data", str_printf("%s.db", jmap_gstring(rqm, "year")), NULL
      ),
      jmap_gstring(rqm, "db")
    );

    Mjson *m = mjson_new();
    return cgi_ok(cgi, m);

  // ------------------------------------------------------- printExercise
  } else if (str_eq(rq, "printExercisxxxxe")) {
    clear_tmp(cgi);
    ext_pdf(
      jmap_gstring(rqm, "tx"),
      path_cat(home, "tmp", "exercise.pdf", NULL),
      ""
    );

    Mjson *m = mjson_new();
    return cgi_ok(cgi, m);

  // ------------------------------------------------------- chpass
  } else if (str_eq(rq, "chpass")) {
    return cgi_change_pass(
      cgi,
      jmap_gstring(rqm, "user"),
      jmap_gstring(rqm, "pass"),
      jmap_gstring(rqm, "newPass")
    );

  // ------------------------------------------------------- backup
  } else if (str_eq(rq, "backup")) {
    clear_tmp(cgi);

    char *name = str_printf("Hconta%s.zip", mk_date());
    ext_zip(
      path_cat(home, "data", NULL),
      path_cat(home, "tmp", name, NULL)
    );

    Mjson *m = mjson_new();
    jmap_pstring(m, "name", name);
    return cgi_ok(cgi, m);

  // ------------------------------------------------------- restoreStart
  } else if (str_eq(rq, "restoreStart")) {
    clear_tmp(cgi);

    LckFile *lck;
    lck = file_wopen(path_cat(home, "tmp", "back.zip", NULL));
    file_close(lck);

    Mjson *m = mjson_new();
    return cgi_ok(cgi, m);

  // ------------------------------------------------------- restoreAppend
  } else if (str_eq(rq, "restoreAppend")) {
    LckFile *lck;
    lck = file_aopen(path_cat(home, "tmp", "back.zip", NULL));
    file_write_bin(lck, b64_decode_bytes(jmap_gstring(rqm, "data")));
    file_close(lck);

    Mjson *m = mjson_new();
    return cgi_ok(cgi, m);

  // ------------------------------------------------------- restoreAbort
  } else if (str_eq(rq, "restoreAbort")) {
    clear_tmp(cgi);

    Mjson *m = mjson_new();
    return cgi_ok(cgi, m);

  // ------------------------------------------------------- restoreEnd
  } else if (str_eq(rq, "restoreEnd")) {
    char *fail = unzip(cgi);

    if (!*fail) {
      char *data = path_cat(home, "data", NULL);
      to_trash(cgi);
      file_del(data);
      file_rename(path_cat(home, "tmp", "data", NULL), data);
      clear_tmp(cgi);
    }

    Mjson *m = mjson_new();
    jmap_pstring(m, "fail", fail);
    return cgi_ok(cgi, m);

  // ------------------------------------------------------- autorestore
  } else if (str_eq(rq, "autorestore")) {
    to_trash(cgi);

    file_del(path_cat(home, "data", NULL));
    ext_unzip(
      path_cat(home, "backups", jmap_gstring(rqm, "file"), NULL),
      home
    );

    Mjson *m = mjson_new();
    return cgi_ok(cgi, m);

  // ------------------------------------------------------- clearTrash
  } else if (str_eq(rq, "clearTrash")) {
    char *path = path_cat(home, "trash", NULL);
    file_del(path);
    file_mkdir(path);

    Mjson *m = mjson_new();
    return cgi_ok(cgi, m);

  // ------------------------------------------------------- restoreTrash
  } else if (str_eq(rq, "restoreTrash")) {
    char *source = path_cat(home, "trash", jmap_gstring(rqm, "file"), NULL);

    if (!file_exists(source)) {
      return cgi_error(cgi, str_printf("Trash back '%s' not found", source));
    }

    to_trash(cgi);
    file_del(path_cat(home, "data", NULL));
    ext_unzip(source, home);

    Mjson *m = mjson_new();
    return cgi_ok(cgi, m);

  // ------------------------------------------------------- logout
  } else if (str_eq(rq, "logout")) {
    ext_zip(
      path_cat(home, "data", NULL),
      path_cat(home, "backups", str_printf("%s.zip", mk_date()), NULL)
    );

    filter_backups(cgi);

    return cgi_del_session(cgi, session_id);

  // ------------------------------------------------------- error!
  } else {
    return cgi_error(cgi, str_printf("'%s': Unknown request", rq));
  }
}

int main (int argc, char **argv) {
  exc_init();
  TRY {
    if (argc != 2) {
      THROW("") "argc must be 2" _THROW
    }
    Cgi *cgi = cgi_new(app_dir, expiration);

    hconta_init(cgi);

    char *rq = argv[1];
    int ix = str_cindex(rq, ':');
    CgiRp *rp;
    if (ix == -1) { //............................................. CONNECTION
      cgi_set_key(cgi, rq);
      rp = cgi_connect(cgi, rq);
   } else if (ix == 0) { //................................... AUTHENTICATION
      char *key = cryp_key(app_name, cgi_klen());
      cgi_set_key(cgi, key);

      char *data = cryp_decryp(key, rq + 1);
      Achar *parts = str_csplit(data, ':');
      rp = cgi_authentication(
        cgi,
        achar_get(parts, 0),
        achar_get(parts, 1),
        *achar_get(parts, 2) == '1'
      );
    } else { //................................................... NORMAL DATA
      char *session_id = str_sub(rq, 0, ix);
      char *key;
      char *connectionId;
      cgi_get_session_data(&key, &connectionId, cgi, session_id);

      if (!*key) {
        cgi_set_key(cgi, "nosession");
        rp = cgi_expired(cgi);
      } else {
        cgi_set_key(cgi, key);
        Mjson *m = json_robject(
          (Json *)cryp_decryp(key, str_sub_end(rq, ix + 1))
        );
        if (
          mjson_has_key(m, "connectionId") &&
          str_cmp(connectionId, jmap_gstring(m, "connectionId"))
        ) {
          cgi_set_key(cgi, "nosession");
          rp = cgi_expired(cgi);
        } else {
          rp = app_process(cgi, session_id, m);
        }
      }
    }

    printf("%s", (char *)rp);
  } CATCH (e) {
    puts (e);
  }_TRY
  return 0;
}

