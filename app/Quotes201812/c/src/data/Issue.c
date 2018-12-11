// Copyright 11-Sept-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#include "data/Issue.h"
#include "data/nicks_db.h"
#include "data/servers_db.h"
#include "data/Server.h"
#include "dmc/ct/Ochar.h"

/* .+.
struct: Issue
  id: char *: _string
  type: enum Issue_t: _int
  cause: char *: _string
*/

/*.-.*/
#include "dmc/ct/Ajson.h"

struct issue_Issue {
  char *id;
  enum Issue_t type;
  char *cause;
};

Issue *issue_new(char *id, enum Issue_t type, char *cause) {
  Issue *this = MALLOC(Issue);
  XNULL(id)
  this->id = id;
  this->type = type;
  XNULL(cause)
  this->cause = cause;
  return this;
}

char *issue_id(Issue *this) {
  XNULL(this)
  return this->id;
}

enum Issue_t issue_type(Issue *this) {
  XNULL(this)
  return this->type;
}

char *issue_cause(Issue *this) {
  XNULL(this)
  return this->cause;
}

Json *issue_to_json(Issue *this) {
  XNULL(this)
  Ajson *serial = ajson_new();
  jarr_astring(serial, this->id);
  jarr_aint(serial, this->type);
  jarr_astring(serial, this->cause);
  return json_warray(serial);
}

Issue *issue_from_json(Json *js) {
  XNULL(js)
  Ajson *serial = json_rarray(js);
  Issue *this = MALLOC(Issue);
  size_t i = 0;
  this->id = jarr_gstring(serial, i++);
  this->type = jarr_gint(serial, i++);
  this->cause = jarr_gstring(serial, i++);
  return this;
}
/*.-.*/

static Issue *_issue_check_quotes(char *nick_id, Aquote *mqs, Aquote *qs) {
  size_t qs_size = aquote_size(qs);
  size_t mqs_size = aquote_size(mqs);

  Quote *q = aquote_get(qs, 0);
  if (quote_open(q) < -1.5) {
    Aquote *new_qs = aquote_new();
    char *date = quote_date(q);
    EACH(mqs, Quote, mq) {
      char *mdate = quote_date(mq);
      if (str_cmp(mdate, date) > 0) {
        aquote_add(new_qs, quote_new(mdate, -2, -2, -2, -2, -2, true));
      } else {
        break;
      }
    }_EACH

    if (aquote_size(new_qs)) {
      aquote_add_arr(new_qs, qs);
      qs = new_qs;
      qs_size = aquote_size(qs);
      nicks_db_set_quotes(nick_id, qs);
    }
  }

  // --------------------------------------------------------- MISSING
  int qs_size1 = qs_size - 1;
  RANGE0(i, qs_size) {
    if (i >= mqs_size) {
      break;
    }
    Quote *q = aquote_get(qs, i);
    Quote *mq = aquote_get(mqs, i);


    char *date = quote_date(q);
    char *dmsg = str_cat(date, ":", NULL);
    if (!str_eq(quote_date(q), quote_date(mq))) {
      if (str_cmp(date, quote_date(mq)) > 0) {
        return issue_new(nick_id, ISSUE_EXTRA, dmsg);
      }
      return issue_new(
        nick_id, ISSUE_MISSING, str_cat(quote_date(mq), ":", NULL)
      );
    }

    if (quote_error(q)) {
      continue;
    }

    double open = quote_open(q);
    double close = quote_close(q);
    double max = quote_max(q);
    double min = quote_min(q);
    if (i < qs_size1) {
      Issue *mk_issue_bn (char *field) {
        return issue_new(nick_id, ISSUE_BEFORE_NOW, str_cat(dmsg, field, NULL));
      }
      Quote *qnext = aquote_get(qs, i + 1);
      if (!quote_error(qnext)) {
        if (abs(open - quote_open(qnext)) > open * 0.20) {
          return mk_issue_bn("open");
        }
        if (abs(close - quote_close(qnext)) > close * 0.20) {
          return mk_issue_bn("close");
        }
        if (abs(max - quote_max(qnext)) > max * 0.20) {
          return mk_issue_bn("max");
        }
        if (abs(min - quote_min(qnext)) > min * 0.20) {
          return mk_issue_bn("min");
        }
      }
    }

    if (open > max) {
      return issue_new(nick_id, ISSUE_MAX, str_cat(dmsg, "open", NULL));
    }
    if (close > max) {
      return issue_new(nick_id, ISSUE_MAX, str_cat(dmsg, "close", NULL));
    }
    if (min > max) {
      return issue_new(nick_id, ISSUE_MAX, str_cat(dmsg, "min", NULL));
    }

    if (open < min) {
      return issue_new(nick_id, ISSUE_MIN, str_cat(dmsg, "open", NULL));
    }
    if (close < min) {
      return issue_new(nick_id, ISSUE_MIN, str_cat(dmsg, "close", NULL));
    }
  }_RANGE

  if (qs_size > mqs_size) {
    qs = aquote_from_it(iquote_take(aquote_to_it(qs), mqs_size));
    nicks_db_set_quotes(nick_id, qs);
  } else if (qs_size < mqs_size) {
    RANGE(i, qs_size, mqs_size) {
      Quote *mq = aquote_get(mqs, i);
      Quote *q = quote_new(quote_date(mq), -1, -1, -1, -1, -1, true);
      aquote_add(qs, q);
    }_RANGE
    nicks_db_set_quotes(nick_id, qs);
  }

  // ------------------------------------------------------------ NONE
  return issue_new(nick_id, ISSUE_NONE, "");

}

Issue *issue_check_quotes(char *nick_id, Aquote *qs) {
  Oaquote *omqs = nicks_db_model_quotes();
  if (oaquote_is_null(omqs)) {
    exc_illegal_state("Model quotes not found");
  }
  Aquote *mqs = oaquote_value(omqs);
  return _issue_check_quotes(nick_id, mqs, qs);
}

static Issue *check(char *nick_id, char *nick_name) {
  // --------------------------------------------------- ------ SERVER
  char *msg = "";
  EACH(servers_db_list(), Server, s) {
    bool missing = true;
    EACHI(mchar_keys(server_nicks(s)), char, id) {
      if (str_eq(id, nick_id)) {
        missing = false;
        break;
      }
    }_EACH
    if (missing) {
      msg = server_name(s);
      break;
    }
  }_EACH
  if (*msg) {
    return issue_new(nick_id, ISSUE_SERVER, msg);
  }

  // ----------------------------------------------------------- EMPTY
  char *model_id = nicks_db_model();
  char *model_name = nicks_db_model_name();
  Oaquote *omqs = nicks_db_model_quotes();
  if (oaquote_is_null(omqs)) {
    return issue_new(model_id, ISSUE_EMPTY, str_cat(model_name, ".db", NULL));
  }
  Aquote *mqs = oaquote_value(omqs);
  if (!aquote_size(mqs)) {
    return issue_new(model_id, ISSUE_EMPTY, str_cat(model_name, ".db", NULL));
  }

  Oaquote *oqs = nicks_db_quotes(nick_id);
  if (oaquote_is_null(oqs)) {
    return issue_new(nick_id, ISSUE_EMPTY, str_cat(nick_name, ".db", NULL));
  }
  Aquote *qs = oaquote_value(oqs);
  if (!aquote_size(qs)) {
    return issue_new(nick_id, ISSUE_EMPTY, str_cat(nick_name, ".db", NULL));
  }

  return _issue_check_quotes(nick_id, mqs, qs);
}

Issue *issue_check(char *nick_id) {
  // ---------------------------------------------------------- EXISTS
  Ochar *onick_name = nicks_db_name(nick_id);
  if (ochar_is_null(onick_name)) {
    return issue_new(nick_id, ISSUE_EXISTS, "");
  }
  return check(nick_id, ochar_value(onick_name));
}

#define TY Issue
#define FN issue
#include "dmc/tpl/topt.c"
#include "dmc/tpl/tarr.c"
#undef TY
#undef FN

Oissue *issue_search(void) {
  EACH(nicks_db_list(), Nick, n) {
    Issue *i = check(nick_id(n), nick_name(n));
    if (i->type != ISSUE_NONE) {
      return oissue_new(i);
    }
  }_EACH
  return oissue_null();
}

char *issue_annotate(Aquote *qs, Issue *i) {
  char *date = issue_cause(i);
  int ix = str_cindex(date, ':');
  date = str_sub(date, 0, ix);
  Buf *bf = buf_new();
  bool not_annotated = true;
  EACH(qs, Quote, q) {
    if (str_cmp(quote_date(q), date) <= 0 && not_annotated) {
      buf_add(bf, "% ******************\n");
      not_annotated = false;
    }
    buf_add(bf, quote_to_str(q));
    buf_cadd(bf, '\n');
  }_EACH
  return buf_to_str(bf);
}
