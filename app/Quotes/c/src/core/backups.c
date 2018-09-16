// Copyright 05-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "core/backups.h"
#include "dmc/ext.h"
#include "dmc/Date.h"
#include "dmc/b64.h"

static char *mk_date(void) {
  return date_format(date_now(), "%Y%m%d");
}

static char *mk_date2(void) {
	Date t = date_now();
  return str_printf("%s-%ld", date_format(t, "%Y%m%d"), t);
}

static Achar *read_trash(void) {
  Achar *fs = file_dir(path_cat(cgi_home(), "trash", NULL));
  Achar *rs = achar_new();
  EACH(fs, char, f) {
    achar_add(rs, path_name(f));
  }_EACH
	return rs;
}

static void to_trash(void) {
  char *date = mk_date2();
  char *source = path_cat(cgi_home(), "data", NULL);
  char *target = path_cat(
    cgi_home(), "trash", str_printf("%s.zip", date), NULL
  );
  ext_zip(source, target);
}

static void clear_tmp(void) {
  char *dir = path_cat(cgi_home(), "tmp", NULL);
  file_del(dir);
  file_mkdir(dir);
}

static char *unzip(char *app_name, char *data_version) {
  char *source = path_cat(cgi_home(), "tmp", "back.zip", NULL);
  char *target = path_cat(cgi_home(), "tmp", NULL);
  char *fail = "";
  TRY {
    ext_unzip(source, target);
  } CATCH(e) {
    fail = "restore:unzip";
  }_TRY
  if (*fail) {
    return fail;
  }

  source = path_cat(cgi_home(), "tmp", "data", "version.txt", NULL);
  if (!file_exists(source)) {
    return "restore:version does not exist";
  }
  char *good_version =str_printf(
    "%s\nData version: %s\n", app_name, data_version
  );
  char *version = file_read(source);
  if (!str_eq(version, good_version)) {
    return "restore:version is wrong";
  }
  return "";
}

//    ________________
CgiRp *backups_process(char *app_name, char* data_version, Mjson *mrq) {
//    TTTTTTTTTTTTTTTT
  char *home = cgi_home();
  char *rq = jmap_gstring(mrq, "rq");

  // -------------------------------------------------------------- backup
  if (str_eq(rq, "backup")) {
    clear_tmp();
    char *name = str_printf("%sBackup%s.zip", app_name, mk_date());

    ext_zip(
      path_cat(home, "data", NULL),
      path_cat(home, "tmp", name, NULL)
    );

    Mjson *m = mjson_new();
    jmap_pstring(m, "name", name);
    return cgi_ok(m);
  }

  // -------------------------------------------------_------ restoreStart
  if (str_eq(rq, "restoreStart")) {
    clear_tmp();
    LckFile *lck = file_wopen(path_cat(home, "tmp", "back.zip", NULL));
    file_close(lck);
    return cgi_ok(mjson_new());
  }

  // ------------------------------------------------------- restoreAppend
  if (str_eq(rq, "restoreAppend")) {
    LckFile *lck = file_aopen(path_cat(home, "tmp", "back.zip", NULL));
    file_write_bin(lck, b64_decode_bytes(jmap_gstring(mrq, "data")));
    file_close(lck);
    return cgi_ok(mjson_new());
  }

  // -------------------------------------------_------------ restoreAbort
  if (str_eq(rq, "restoreAbort")) {
    clear_tmp();
    return cgi_ok(mjson_new());
  }

  // ---------------------------------------------------------- restoreEnd
  if (str_eq(rq, "restoreEnd")) {
    char *fail = unzip(app_name, data_version);

    if (!*fail) {
      char *data = path_cat(home, "data", NULL);
      to_trash();
      file_del(data);
      file_rename(path_cat(home, "tmp", "data", NULL), data);
      clear_tmp();
    }

    Mjson *m = mjson_new();
    jmap_pstring(m, "fail", fail);
    return cgi_ok(m);
  }

  // --------------------------------------------------------- trashList
  if (str_eq(rq, "trashList")) {
    Mjson *m = mjson_new();
    jmap_parray(m, "list", (Arr *)read_trash(), (TO_JSON)json_wstring);
    return cgi_ok(m);
  }

  // ---------------------------------------------------------- clearTrash
  if (str_eq(rq, "clearTrash")) {
    char *path = path_cat(home, "trash", NULL);
    file_del(path);
    file_mkdir(path);
    return cgi_ok(mjson_new());
  }

  // -------------------------------------------------------- restoreTrash
  if (str_eq(rq, "restoreTrash")) {
    char *source = path_cat(home, "trash", jmap_gstring(mrq, "file"), NULL);

    if (!file_exists(source)) {
      return cgi_error(str_printf("Trash back '%s' not found", source));
    }

    to_trash();
    file_del(path_cat(home, "data", NULL));
    ext_unzip(source, home);

    return cgi_ok(mjson_new());
  }

  THROW("") "'%s': Unknown request in backups", rq _THROW
  // Unreacheable
  return NULL;
}
