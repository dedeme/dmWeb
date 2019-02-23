// Copyright 18-Oct-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "dmc/cgi.h"
#include "dmc/rnd.h"
#include "dmc/cryp.h"
#include "dmc/date.h"

static size_t klen = 300;
static time_t t_no_expiration = 2592000; // seconds == 30 days
static char *deme_key =
    "nkXliX8lg2kTuQSS/OoLXCk8eS4Fwmc+N7l6TTNgzM1vdKewO0cjok51vcdl"
    "OKVXyPu83xYhX6mDeDyzapxL3dIZuzwyemVw+uCNCZ01WDw82oninzp88Hef"
    "bn3pPnSMqEaP2bOdX+8yEe6sGkc3IO3e38+CqSOyDBxHCqfrZT2Sqn6SHWhR"
    "KqpJp4K96QqtVjmXwhVcST9l+u1XUPL6K9HQfEEGMGcToMGUrzNQxCzlg2g+"
    "Hg55i7iiKbA0ogENhEIFjMG+wmFDNzgjvDnNYOaPTQ7l4C8aaPsEfl3sugiw";

struct cgi_Cgi {
  char *fkey;      // key to encrypt files
  char *key_null;  // key to communications.
  char *home;
  time_t t_expiration;
};

typedef struct cgi_Cgi Cgi;

static Cgi *cgi_null = NULL;

// users is Arr[char]
static void write_users(Arr *users);
static void put_user(const char *user, const char *key, const char *level);
// sessions is Arr[Js]
static void write_sessions(Arr *sessions);


void cgi_init(const char *home, time_t t_expiration) {
  rnd_init();

  cgi_null = malloc(sizeof(Cgi));

  cgi_null->fkey = str_new(deme_key);
  cryp_key(&cgi_null->fkey, strlen(deme_key));
  cgi_null->key_null = NULL;
  cgi_null->home = str_new(home);
  cgi_null->t_expiration = t_expiration;

  if (!file_exists(home)) {
    file_mkdir(home);
  }

  char *fusers = path_cat_new(home, "users.db", NULL);
  if (!file_exists(fusers)) {
    // Arr[char]
    Arr *a = arr_new(free);
    write_users(a);
    put_user("admin", deme_key, "0");
    write_sessions(a);
    free(a);
  }
  free(fusers);
}

void cgi_end(void) {
  if (cgi_null) {
    free(cgi_null->fkey);
    free(cgi_null->key_null);
    free(cgi_null->home);
  }
  free(cgi_null);
}

// User ----------------------------------------------------

// users is Arr[char]
static void write_users(Arr *users) {
  char *path = path_cat_new(cgi_null->home, "users.db", NULL);
  // Arr[Js]
  Arr *tmp = arr_new(free);
  EACH(users, char, udata) {
    arr_push(tmp, js_ws_new(udata));
  }_EACH
  char *js = (char *)js_wa_new(tmp);
  arr_free(tmp);
  cryp_cryp(&js, cgi_null->fkey);

  file_write(path, js);
  free(path);
  free(js);
}

// Returns Arr[char]
static Arr *read_users_new(void) {
  char *path = path_cat_new(cgi_null->home, "users.db", NULL);
  char *tx = file_read_new(path);
  cryp_decryp(&tx, cgi_null->fkey);
  free(path);
  // Arr[Js]
  Arr *tmp = js_ra_new((Js *)tx);
  free(tx);
  // Arr[char]
  Arr *r = arr_new(free);
  EACH(tmp, Js, js) {
    arr_push(r, js_rs_new(js));
  }_EACH
  arr_free(tmp);
  return r;
}

// Users is Arr[char]
static void remove_user(Arr *users, const char *user) {
  int pred(char *row) {
    // Arr[char]
    Arr *parts = str_csplit_new(row, ':');
    int r = str_eq(user, arr_get(parts, 0));
    arr_free(parts);
    return r;
  }

  int ix = arr_index(users, (FPRED)pred);
  if (ix != -1) {
    arr_remove(users, ix);
  }
}

static void del_user(const char *user) {
  // Arr[char]
  Arr *users = read_users_new();
  remove_user(users, user);
  write_users(users);
  arr_free(users);
}


static void put_user(const char *user, const char *key, const char *level) {
  // Arr[char]
  Arr *users = read_users_new();
  remove_user(users, user);
  char *kkey = str_new(key);
  cryp_key(&kkey, klen);
  arr_push(
    users,
    str_cat_new(user, ":", kkey, ":", level, NULL)
  );
  write_users(users);
  arr_free(users);
  free(kkey);
}

// Returns level of "id" or NULL
static char *check_user_new_null(const char *id, const char *key) {
  char *kkey = str_new(key);
  cryp_key(&kkey, klen);

  // Arr[char]
  Arr *users = read_users_new();

  char *r = NULL;
  EACH(users, char, udata) {
    // Arr[char]
    Arr *parts = str_csplit_new(udata, ':');
    if (
      str_eq(arr_get(parts, 0), id) &&
      str_eq(arr_get(parts, 1), kkey)
    ) {
      r = str_new(arr_get(parts, 2));
      arr_free(parts);
      break;
    }
    arr_free(parts);
  }_EACH

  free(kkey);
  arr_free(users);
  return r;
}

static int change_level(const char *user, const char *level) {
  int pred(char *row) {
    // Arr[char]
    Arr *parts = str_csplit_new(row, ':');
    int r = str_eq(user, arr_get(parts, 0));
    arr_free(parts);
    return r;
  }

  // Arr[char]
  Arr *users = read_users_new();
  int ix = arr_index(users, (FPRED)pred);

  if (ix == -1) {
    return 0;
  }

  char *row = arr_get(users, ix);
  // Arr[char]
  Arr *parts = str_csplit_new(row, ':');
  char *new_row = str_cat_new(
    arr_get(parts, 0), ":",
    arr_get(parts, 1), ":",
    level, NULL
  );
  arr_free(parts);
  arr_set(users, ix, new_row);
  write_users(users);

  arr_free(users);
  return 1;
}

static int change_pass(
  const char *user, const char *old_pass, const char *new_pass
) {
  int pred(char *row) {
    char *kold = str_new(old_pass);
    cryp_key(&kold, klen);
    // Arr[char]
    Arr *parts = str_csplit_new(row, ':');
    int r = str_eq(arr_get(parts, 0), user) &&
            str_eq(arr_get(parts, 1), kold);
    arr_free(parts);
    free(kold);
    return r;
  }

  // Arr[char]
  Arr *users = read_users_new();
  int ix = arr_index(users, (FPRED)pred);

  if (ix == -1) {
    return 0;
  }

  char *knew = str_new(new_pass);
  cryp_key(&knew, klen);
  char *row = arr_get(users, ix);
  // Arr[char]
  Arr *parts = str_csplit_new(row, ':');
  char *new_row = str_cat_new(
    arr_get(parts, 0), ":",
    knew, ":",
    arr_get(parts, 2), NULL
  );
  arr_free(parts);
  free(knew);
  arr_set(users, ix, new_row);
  write_users(users);

  arr_free(users);
  return 1;
}

// Session -------------------------------------------------

// sessions is Arr[Js] (Where Js is a json array)
static void write_sessions(Arr *sessions) {
  char *path = path_cat_new(cgi_null->home, "sessions.db", NULL);
  char *data = (char *)js_wa_new(sessions);
  cryp_cryp(&data, cgi_null->fkey);
  file_write(path, data);
  free(path);
  free(data);
}

// Returns Arr[Js] (Where Js is a json array)
static Arr *read_sessions_new(void) {
  char *path = path_cat_new(cgi_null->home, "sessions.db", NULL);
  char *data = file_read_new(path);
  free(path);
  cryp_decryp(&data, cgi_null->fkey);
  Arr *r = js_ra_new((Js*)data);
  free(data);
  return r;
}

// If expiration is 0 tNoExpiration is used
static void add_session(
  const char *session_id, const char *user, const char *key, time_t expiration
) {
  // row is a Js array
  int pred(Js *jsrow) {
    // Arr[js]
    Arr *row = js_ra_new(jsrow);
    char *u = js_rs_new(arr_get(row, 1));
    int r = str_eq(u, user);
    arr_free(row);
    free(u);
    return r;
  }

  // Arr[Js] (Where Js is a json array)
  Arr *sessions = read_sessions_new();
  int ix = arr_index(sessions, (FPRED)pred);
  if (ix != -1) {
    arr_remove(sessions, ix);
  }

  time_t lapse = expiration ? expiration : t_no_expiration;
  time_t time = date_now() + lapse;

  // Arr[Js]
  Arr *new_row = arr_new(free);
  arr_push(new_row,  js_ws_new(session_id));  //0
  arr_push(new_row,  js_ws_new(user));        //1
  arr_push(new_row,  js_ws_new(key));         //2
  arr_push(new_row,  js_ws_new(""));          //3
  arr_push(new_row,  date_to_js_new(time));   //4
  arr_push(new_row,  date_to_js_new(lapse));  //5

  arr_push(sessions, js_wa_new(new_row));
  write_sessions(sessions);

  arr_free(new_row);
  arr_free(sessions);
}

// In session.db:
// Fields are: sessionId:key:time:lapse
// If identification fails returns ""
static void read_session(
  char **key_new, char** con_id_new, const char *session_id
) {
  time_t now = date_now();

  // row is a Js array
  int filter(Js *jsrow) {
    // Arr[js]
    Arr *row = js_ra_new(jsrow);
    int r = (date_from_js(arr_get(row, 4)) >= now);
    arr_free(row);
    return r;
  }

  // row is a Js array
  int pred(Js *jsrow) {
    // Arr[js]
    Arr *row = js_ra_new(jsrow);
    char *s_id = js_rs_new(arr_get(row, 0));
    int r = str_eq(s_id, session_id);
    arr_free(row);
    free(s_id);
    return r;
  }

  // Arr[Js] (Where Js is a json array)
  Arr *sessions = read_sessions_new();

  arr_filter(sessions, (FPRED)filter);
  write_sessions(sessions);

  int ix = arr_index(sessions, (FPRED)pred);

  if (ix == -1) {
    *key_new = str_new("");
    *con_id_new = str_new("");
  } else {
    // Arr[js]
    Arr *row = js_ra_new(arr_get(sessions, ix));
    *key_new = js_rs_new(arr_get(row, 2));
    *con_id_new = js_rs_new(arr_get(row, 3));
    arr_free(row);
  }

  arr_free(sessions);
}

static void del_session(const char *session_id) {
  // row is a Js array
  int pred(Js *jsrow) {
    // Arr[js]
    Arr *row = js_ra_new(jsrow);
    char *s_id = js_rs_new(arr_get(row, 0));
    int r = str_eq(s_id, session_id);
    arr_free(row);
    free(s_id);
    return r;
  }

  // Arr[Js] (Where Js is a json array)
  Arr *sessions = read_sessions_new();
  int ix = arr_index(sessions, (FPRED)pred);

  if (ix != -1) {
    arr_remove(sessions, ix);
    write_sessions(sessions);
  }

  arr_free(sessions);
}

static void set_connection_id(const char *session_id, const char *con_id) {
  // row is a Js array
  int pred(Js *jsrow) {
    // Arr[js]
    Arr *row = js_ra_new(jsrow);
    char *s_id = js_rs_new(arr_get(row, 0));
    int r = str_eq(s_id, session_id);
    arr_free(row);
    free(s_id);
    return r;
  }

  // Arr[Js] (Where Js is a json array)
  Arr *sessions = read_sessions_new();
  int ix = arr_index(sessions, (FPRED)pred);

  if (ix != -1) {
    // Arr[js]
    Arr *row = js_ra_new(arr_get(sessions, ix));
    arr_set(row, 3, js_ws_new(con_id));
    arr_set(sessions, ix, js_wa_new(row));
    arr_free(row);
    write_sessions(sessions);
  }

  arr_free(sessions);
}

// Public interface ----------------------------------------

int cgi_klen(void) {
  return klen;
}

char *cgi_home(void) {
  if (!cgi_null) {
    FAIL("'cgi' has not been intialized")
  }
  return cgi_null->home;
}

void cgi_set_key(const char *k) {
  if (!cgi_null) {
    FAIL("'cgi' has not been intialized")
  }
  free(cgi_null->key_null);
  cgi_null->key_null = str_new(k);
}

void cgi_get_session_data(
  char **key, char **connectionId, const char *session_id
) {
  if (!cgi_null) {
    FAIL("'cgi' has not been intialized")
  }
  read_session(key, connectionId, session_id);
}

void cgi_add_user(
  const char *admin, const char *akey,
  const char *user, const char *ukey, const char *level
){
  if (!cgi_null) {
    FAIL("'cgi' has not been intialized");
  }
  // Map[Js]
  Map *m = map_new(free);
  char *alevel = check_user_new_null(admin, akey);
  if (alevel && str_eq(alevel, "0")) {
    put_user(user, ukey, level);
    map_put(m, "ok", js_wb_new(1));
    free(alevel);
  } else {
    map_put(m, "ok", js_wb_new(0));
  }
  cgi_ok(m);
  map_free(m);
}

void cgi_del_user(const char *admin, const char *akey, const char *user) {
  if (!cgi_null) {
    FAIL("'cgi' has not been intialized");
  }
  // Map[Js]
  Map *m = map_new(free);
  char *alevel = check_user_new_null(admin, akey);
  if (alevel && str_eq(alevel, "0")) {
    del_user(user);
    map_put(m, "ok", js_wb_new(1));
    free(alevel);
  } else {
    map_put(m, "ok", js_wb_new(0));
  }
  cgi_ok(m);
  map_free(m);
}

void cgi_change_level(
  const char *admin, const char *akey, const char *user, const char *level
) {
  if (!cgi_null) {
    FAIL("'cgi' has not been intialized");
  }
  // Map[Js]
  Map *m = map_new(free);
  char *alevel = check_user_new_null(admin, akey);
  if (alevel && str_eq(alevel, "0")) {
    if (change_level(user, level)) {
      map_put(m, "ok", js_wb_new(1));
    } else {
      map_put(m, "ok", js_wb_new(0));
    }
    free(alevel);
  } else {
    map_put(m, "ok", js_wb_new(0));
  }
  cgi_ok(m);
  map_free(m);
}

void cgi_change_pass(const char *user, const char *key, const char *new_key) {
  if (!cgi_null) {
    FAIL("'cgi' has not been intialized");
  }
  // Map[Js]
  Map *m = map_new(free);
  if (change_pass(user, key, new_key)) {
    map_put(m, "ok", js_wb_new(1));
  } else {
    map_put(m, "ok", js_wb_new(0));
  }
  cgi_ok(m);
  map_free(m);
}

void cgi_del_session(const char *session_id) {
  if (!cgi_null) {
    FAIL("'cgi' has not been intialized");
  }
  // Map[Js]
  Map *m = map_new(free);
  del_session(session_id);
  cgi_ok(m);
  map_free(m);
}

void cgi_authentication(const char *user, const char *key, int expiration) {
  if (!cgi_null) {
    FAIL("'cgi' has not been intialized");
  }
  // Map[Js]
  Map *m = map_new(free);

  char *level = check_user_new_null(user, key);
  if (level) {
    char *session_id = cryp_genk_new(klen);
    char *key = cryp_genk_new(klen);
    add_session(
      session_id, user, key, expiration ? cgi_null->t_expiration : 0
    );
    map_put(m, "level", js_ws_new(level));
    map_put(m, "sessionId", js_ws_new(session_id));
    map_put(m, "key", js_ws_new(key));

    free(level);
    free(session_id);
    free(key);
  } else {
    map_put(m, "level", js_ws_new(""));
    map_put(m, "sessionId", js_ws_new(""));
    map_put(m, "key", js_ws_new(""));
  }

  cgi_ok(m);
  map_free(m);
}

void cgi_connect(const char *session_id) {
  if (!cgi_null) {
    FAIL("'cgi' has not been intialized");
  }
  // Map[Js]
  Map *m = map_new(free);

  char *k = cryp_genk_new(klen);
  set_connection_id(session_id, k);
  free(k);

  char *key_new;
  char *connection_id_new;
  read_session(&key_new, &connection_id_new, session_id);

  map_put(m, "key", js_ws_new(key_new));
  map_put(m, "connectionId", js_ws_new(connection_id_new));
  cgi_ok(m);
  free(key_new);
  free(connection_id_new);
  map_free(m);
}

// data is Map[Js]
void cgi_ok(Map *data) {
  if (!cgi_null) {
    FAIL("'cgi' has not been intialized");
  }
  if (!cgi_null->key_null) {
    FAIL("this->key_null not set")
  }
  char *msg = (char *)js_wo_new(data);

  cryp_cryp(&msg, cgi_null->key_null);
/*
  // For debug
  FILE *tmp = fopen("data/tmp.txt", "w");
  fputs(msg, tmp);
  fclose(tmp);
*/

  printf("%s", msg);
  free(msg);
}

void cgi_empty(void) {
  // Map[Js]
  Map *m = map_new(free);
  cgi_ok(m);
  free(m);
}

void cgi_error(const char *msg) {
  // Map[Js]
  Map *m = map_new(free);
  map_put(m, "error", js_ws_new(msg));
  cgi_ok(m);
  map_free(m);
}

void cgi_expired(void) {
  // Map[Js]
  Map *m = map_new(free);
  map_put(m, "expired", js_wb_new(1));
  cgi_ok(m);
  map_free(m);
}

