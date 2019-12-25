// Copyright 05-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Trash management.

#ifndef IO_TRASH_H
  #define IO_TRASH_H

#include "dmc/async.h"

///
void trash_init (void);

/// Arr[char]
Arr *trash_list (void);

/// Zips directory data in trash directory.
void trash_zip_data (void);

/// Clears trash directory
void trash_clear (void);

/// Restores a file in trash directory.
void trash_restore (char *file);

#endif
// Copyright 08-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Management of 'calendar&#46;db'

#ifndef IO_CALENDAR_H
  #define IO_CALENDAR_H

#include "dmc/async.h"
#include "data/Timetable.h"
#include "DEFS.h"

///
void calendar_init (void);

/// Returns general time table of market
Timetable *calendar_general (void);

/// Sets general time table of market.
void calendar_set_general (Timetable *time_table);

/// Returns Arr[char]
Arr *calendar_holidays (void);

/// 'holidays' is Arr[char]
void calendar_set_holidays (Arr *holidays);

/// Returns Arr[MarketDay]
Arr *calendar_special_days (void);

/// 'special_days' is Arr[MarketDay]
void calendar_set_special_days (Arr *special_days);

///
int calendar_is_open (time_t date_time);

#endif
// Copyright 24-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// HTTP readings.

#ifndef IO_NET_H
  #define IO_NET_H

#include "dmc/async.h"
#include "data/Server.h"
#include "data/Rconf.h"
#include "data/EMsg.h"

/// Returns Opt[Arr[NickClose]]<p>
/// Arr[NickClose] can not contain some nick. In such case an error annotation
/// will be done in Log.
Opt *net_server_daily_read (Server *this);


/// Reads a daily conf returning an Opt[Arr[DailyEntry]].<p>
/// Fails are written in Log.
///   conf: Daily configuration
Opt *net_read_daily (Rconf *conf);

/// Returns Opt[Arr[Quote]]<p>
///   nick_id: Nick identifier
///   return: Opt[Arr[Quote]]. If Arr[quote] has less than 10 elements an
///           annotation is done in Log.
Opt *net_server_historic_read (Server *this, int nick_id);

/// Reads a historic conf returning an Opt[Arr[HistoricEntry]].<p>
/// Fails are written in Log.
///   conf: Historic configuration
///   code: Company code.
Opt *net_read_historic (Rconf *conf, char *code);

/// Updates quotes of nk_id
///   return: EMsg with values:
///     MSG_OK, "" -> No error
///     MSG_ERROR, "nick" -> Nick not found
///     MSG_ERROR, "server" -> Wrong server data
///     MSG_ERROR, "net" -> Connection fail
///     MSG_ERROR, "server/quotes" -> Wrong server data and quotes corrected
///     MSG_ERROR, "net/quotes" -> Connection fail and quotes corrected
///     MSG_WARNING, "quotes" -> Quotes corrected
EMsg *net_update_historic(AsyncActor *ac, int nk_id);

/// Updates daily quotes. Possible errors are annotated in log.
void net_update_daily (AsyncActor *ac);

#endif
// Copyright 28-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Daily companies data.

#ifndef IO_DAILYDB_H
  #define IO_DAILYDB_H

#include "dmc/async.h"

///
void dailydb_init (void);

/// Called when activating
void dailydb_reset (void);

/// Called when activating (after reset) and when active
void dailydb_update (void);

/// Returns daily data of selected companies. Data is:
///   [
///     [
///       (0) Nick: String -- Company nick name
///       (1) Quotes: [
///             [
///               (0) Hour: String -- Quote hour
///               (1) Quote: Double -- Retrieved from accdb_dailyq_read
///             ] -- One for each valid quote reading (values < 1 are discarded)
///           ]
///       (2) Stocks: Double -- Stocks in portfolio or 0
///       (3) Price: Double -- Buy price or 0
///       (4) ref: Double -- Buy-Sell signal (reference)
///                          (> 0) Support (buying)
///                          (< 0) -> Resitence (selling)
///                          (= 0) -> Not operation
///    ] -- One for each selected company
///  ]
Js *dailydb_cos (void);

/// Add-remove companies and update its inclusion in portfolio.
void dailydb_update_charts (void);

#endif
// Copyright 03-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// IO utilities of general caracter, including initialization.

#ifndef IO_IO_H
  #define IO_IO_H

#include "dmc/async.h"

/// Initializes 'io'. It must be called at the beginning of application.
void io_init (void);

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

/// Returns millisecons since epoch (Used from zip files)
char *io_time_stamp (void);

#endif
// Copyright 12-Jul-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Manager data base

#ifndef IO_MANAGERDB_H
  #define IO_MANAGERDB_H

#include "dmc/async.h"
#include "data/Manager.h"

///
void managerdb_init ();

///
Manager *managerdb_read (void);

///
void managerdb_write (Manager *mg);

///
ModelParams *managerdb_default (void);

///
ModelParams *managerdb_nick (char *nick);

/// Adds or changes ModelParams of 'nick' to 'managerdb_default'
void managerdb_set_nick_default (char *nick);

/// Adds or changes ModelParams of 'nick'. If model does not exist or 'params'
/// is not correct, returns '1'.
///   nick: nick to change. If its value is "", change default parameters.
///   model: Model name.
///   params: Model parameters
///   return: 0 if success, 1 if fails.
int managerdb_set_nick (char *nick, char *model, Darr *params);

#endif
// Copyright 03-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Log management ('log&#46;txt')

#ifndef IO_LOG_H
  #define IO_LOG_H

#include "dmc/async.h"

/// Intializes 'Log'
void log_init (void);

/// Writes an error
void log_error (char *msg);

/// Writes a warning
void log_info (char *msg);

/// Writes a exception.
void log_exception (Exc *ex);

/// Clears log
void log_clear (void);

/// Returns an array of messages 'JSONized'
Js *log_to_js (void);

#endif
// Copyright 12-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Fleas data base.

#ifndef IO_FLEASDB_H
  #define IO_FLEASDB_H

#include "dmc/async.h"
#include "data/Rs.h"
#include "data/Rank.h"

/// Initilizes fleas data base.
void fleasdb_init();

/// Returns Arr[char]
Arr *fleasdb_model_dates (char * model);

/// Returns Js -> Arr[RsWeb]
Js *fleasdb_model_read_js (char *model, char *date);

/// 'rs' is Arr[RsWeb].<p>
/// 'params' has the following structure:
///   [
///     (0) params0: [
///         (0) prefix: String -- Some text before number or ""
///         (1) multiplicator: Double -- (normaly '1', for % '100'
///         (2) decimal: Int -- Decimal positions
///         (3) postfix: String -- Some text after number or ""
///       ]
///     (-) params-: [...]
///     --- So many as flea parameters
///   ]
void fleasdb_model_write (char *model, Js *params, char *date, Arr *rs);

/// Returns Arr[RsBests]
Arr *fleasdb_bests_read (char *model);

/// Returns Js -> Arr[RsBests]
Js *fleasdb_bests_read_js (char *model);

///
void fleasdb_bests_add (char *model, RsBests *rs);

/// Returns Js -> Arr[char]
Js *fleasdb_charts_read_nicks_js (char *model);

/// Returns Js -> Opt[RsChart]
Js *fleasdb_charts_read_js (char *model, char *nick);

///
void fleasdb_charts_write (char *model, RsCharts *rs);

/// Returns Arr[RsChampions]
Arr *fleasdb_champions_read (int nparams);

/// Returns Js -> Arr[RsChampions]
Js *fleasdb_champions_read_js (int nparams);

/// Returns Js -> Opt[RsChart]
Js *fleasdb_champions_chart_read_js (
  int nparams, char *model, char *nick, char *flea
);

/// If 'rs' is duplicate, sets value. If not adds it.
void fleasdb_champions_add (RsChampions *rs);

/// 'rss' is Arr[RsChampions]
void fleasdb_champions_write (int nparams, Arr *rss);

/// Calculate results of 'f'.
///   model : Model to make calculations.
///   f     : Flea
///   result: Opt<RsChampions>. Results of 'f'
Opt *fleasdb_rsChampions(char *model, Flea *f);

/// Returns Arr[Arr[RankAssets]]. Assets table to make charts of ranking.
///   ranking: (Arr[RankAssetsEntry]) A row will be generated for each
///            RankAssetsEntry.
Arr *fleasdb_ranking_assets (Arr *ranking);

/// Writes a fleas log entry. 'msg' can not finish in '\n'
void fleasdb_flog_write (char *msg);

/// Returns Js->Arr[char]. Reads fleas log.
Js *fleasdb_flog_to_js (void);

/// Clears fleas log
void fleasdb_flog_clear (void);

#endif
// Copyright 06-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Management of 'nicks&#46;db'

#ifndef IO_NICKS_H
  #define IO_NICKS_H

#include "dmc/async.h"
#include "data/Nick.h"

/*--*/

///
typedef struct nicks_Nicks Nicks;

///
Js *nicks_to_js(Nicks *this);

///
Nicks *nicks_from_js(Js *js);

/*--*/

/// Initializes data base.
void nicks_init (void);

/// Returns id of nick model or -1 if it has not been set.
int nicks_model (void);

/// Sets nick model
void nicks_set_model(int nk_id);

/// Arr[Nick]
Arr *nicks_list (void);

/// Adds a nick if it is not duplicated and returns 1. Otherwise returns 0.
int nicks_add(char *nk_name);

/// Removes nick with id 'id' if it exists
void nicks_del(int nk_id);

/// Returns Opt[Nick] with the nick which id is 'id'. If it does not exist
/// returns 'opt_empty()'
Opt *nicks_get(int nk_id);

/// Modifies nick if 'nick_name(nick)' is not duplicated and returns 1.
/// Otherwise returns 0
int nicks_modify(Nick *nick);

/// Set the nick with 'nick_id' selected on/off. If nick_id does not exist,
/// it does nothing
///   value: is 0 or 1.
void nicks_set_selected(int nk_id, int value);

#endif
// Copyright 26-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Daily servers selector.

#ifndef IO_SBOX_H
  #define IO_SBOX_H

#include "dmc/async.h"


///
void sbox_init ();

/// Generates next server
void sbox_next ();

/// Returns Opt[Server] Current server
Opt *sbox_get ();

#endif
// Copyright 05-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Backups management.

#ifndef IO_BACKUPS_H
  #define IO_BACKUPS_H

#include "dmc/async.h"

///
void backups_init (void);

/// Arr[char]
Arr *backups_list (void);

/// Makes a backup in a zip file and returns its name (including extension
/// .zip)
char *backups_make (void);

/// Makes an automatic backup
void backups_make_automatic (void);

/// Starts restore process, creating an empty "back.zip".
void backups_restore_start (void);

/// Appends binary data in B64 format to file "back.zip" created with
/// 'backups_restore_start'
void backups_restore_append (char *data);

/// Aborts a restore process
void backups_restore_abort (void);

/// Finalizes restore process an returning: "" if there is no error,
/// "missing" if there is not 'version.txt' in the backup file or "wrong"
/// the content of 'version.txt' is not valid.
char *backups_restore_end (void);

/// Restores an automatic backup
void backups_autorestore (char *file);

#endif
// Copyright 23-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Accounting data base.

#ifndef IO_ACCDB_H
  #define IO_ACCDB_H

#include "dmc/async.h"
#include "data/Qmatrix.h"
#include "data/Acc.h"
#include "data/Rs.h"

///
void accdb_init (void);

/// Returns a QmatrixValues of quotes, which indices match nicks indices. Values
/// missing are set to '-1'.<br>
/// 'nicks' is Arr[char]
QmatrixValues accdb_dailyq_read (Arr *nicks);

/// if 'nick' is missing, returns -1
double accdb_dailyq_read_nick (char *nick);

/// 'quotes' is Js-> Map[Js->double]
void accdb_dailyq_write (Js *quotes);

/// Returns results including daily quotes and using the current accouning
/// flea model.
RsHistoric *accdb_historic_with_dailyq (char *nick);

/// Returns results not including daily quotes and using the current accouning
/// flea model.
RsHistoric *accdb_historic (char *nick);

/// Returns Arr[AccEntry] with every entry (From before to after)
Arr *accdb_diary_read (void);

/// Returns Js->[Arr[AccEntry]] with last year entries (From after to before)
Js *accdb_diary_read_js (void);

///
void accdb_diary_add (AccEntry *entry);

///
void accdb_diary_remove (char *year, int id);

/// Updates portfolio with current quotes and broker references
void accdb_pf_update (AccPf *pf);

/// Returns [Js-> Arr[char, Darr]].
/// Each row is 'date, Profits [total, acc, risk]'
Js *accdb_profits_read_js (void);

///
void accdb_profits_add (char *date, double total, double acc, double risk);

/// Returns profits with a new record added, but without saving.<p>
/// Returns [Js-> Arr[char, Darr]].
/// Each row is 'date, Profits [total, acc, risk]'
Js *accdb_profits_with (char *date, double total, double acc, double risk);

#endif
// Copyright 10-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Management of companies quotes (Directory quotes)

#ifndef IO_QUOTES_H
  #define IO_QUOTES_H

#include "dmc/async.h"
#include "data/Quote.h"
#include "data/EMsg.h"
#include "data/Qmatrix.h"

///
void quotes_init (void);

/// Returns quotes of 'nick_name'.
///   nick_name: Name of nick (e.g. TEF)
///   return: Arr[Quote]. If an error results, the array is empty.
///           Order of quotes is from after to before.
Arr *quotes_read (char *nick_name);

/// Writes 'qs' of 'nick_name'
///   nick_name: Name of nick (e.g. TEF)
///   qs: It is Arr[Quote]. Its order is from after to before.
void quotes_write (char *nick_name, Arr *qs);

/// Creates a new empty file if it does not already exist.
///   nick_name: Name of nick (e.g. TEF)
void quotes_add (char *nick_name);

/// Deletes a file if it exist.
///   nick_name: Name of nick (e.g. TEF)
void quotes_del (char *nick_name);

/// Changes file 'old_name' by 'new_name' if 'old_name' exists
void quotes_modify (char *old_name, char *new_name);

/// Returns is Tp[EMsg, char]
///   nick_id Nick identifier.
///   returns: Tp of:
///     EMsg: whith values:
///       MSG_OK, "" -> No error
///       MSG_WARNING, "quotes" -> Warning: quotes with error
///       MSG_WARNING, "number" -> Warning: Bad quotes number
///       MSG_ERROR, "io" -> Error: File not found
///       MSG_ERROR, "syntax" -> Error: Syntax error in lines
///       MSG_ERROR, "empty" -> Error: There is no quote
///     char: The text file or "" if it does not exists.
Tp *quotes_editor_read(int nick_id);

/// Sets quotes of a company (inclusive quotes model)
///   nick_id: Nick identifier.
///   qs_text: Text with quotes
///   return: An EMsg with values:
///       MSG_OK, "" -> No error
///       MSG_WARNING, "quotes" -> Warning: quotes with error
///       MSG_WARNING, "number" -> Warning: Bad quotes number
///       MSG_ERROR, "io" -> Error: File not found
///       MSG_ERROR, "syntax" -> Error: Syntax error in lines
///       MSG_ERROR, "empty" -> Error: There is no quote
///       MSG_ERROR, "model" -> Error: Nick model was not defined
///       <i>NOTE: MSG_OK and MSG_WARNING modifies data, MSG_ERROR not</i>.
EMsg *quotes_editor_set_quotes(int nick_id, char *qs_text);

/// Returns an Arr[char] with dates of flea model, from before to after. If
/// data have errors returns an empty array.
Arr *quotes_dates (void);

/// Returns a Opt[Qmatrix] with closes of selected companies, from before to
/// after
Opt *quotes_closes (void);

/// Returns a Opt[Qmatrix] with opens of selected companies, from before to
/// after
Opt *quotes_opens (void);

/// Returns a Opt[char] with the last date of company model. If nick model is
/// not defined, returns the last date of the first company. If there is no
/// company or model has not quotes returns opt_empty.
Opt *quotes_last_date (void);

/// Returns Map[Js->double]. Keys are nicks_names. If there is some fail
/// returns an empty Map.
Map *quotes_last_quotes (void);

/// Returns Map[Js->int]. (nick_name->volume average of VOLUME_QUOTES days).<br>
/// Nicks are retrieved from quotes data base and can be some nick missing
/// relative to nicks data base.
Js *quotes_volume (void);

#endif
// Copyright 28-Nov-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#ifndef IO_RANK_H
  #define IO_RANK_H

#include "dmc/async.h"
#include "data/RankEntry.h"
#include "data/RankAssetsEntry.h"
#include "data/RankEvalEntry.h"

/// Initializes data base.
void rank_init (void);

/// Undate ranking database.
void rank_update (void);

/// Returns dates of historic ranking or throw an exception if the historic
/// ranking is empty.
///   return: Arr<char>. Dates in format "yyyymmdd" sorted ascendingly.
Arr *rank_dates (void);

/// Returns fleas of 'date' or
/// fleas of the last date if 'date' is not found or
/// throws an exception if last date is not found.
///   date  : Date to find.
///   return: Arr<RankAssetsEntry> Descendingly sorted
Arr *rank_fleas (char *date);

/// Returns fleas of day previous to 'date' or
/// fleas of day previous of the last date if 'date' is not found or
/// [] if no date is found.
///   date  : Date to find.
///   return: Arr<RankAssetsEntry>
Arr *rank_fleas_previous (char *date);

#endif
// Copyright 04-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Management of 'conf&#46;db'

#ifndef IO_CONF_H
  #define IO_CONF_H

#include "dmc/async.h"

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

/// Returns model for fleas->bests and fleas-charts
char *conf_fleas_model (void);

/// Sets model for fleas->bests and fleas-charts
void conf_set_fleas_model (char *model);

/// Returns the parameter number selected for champions
int conf_champions_nparams (void);

/// sets the parameter number selected for champions
void conf_set_champions_nparams (int nparams);

/// Returns '1' if page acc->companies shows all companies and '0' if only
/// shows those in portfolio.
int conf_acc_all_cos (void);

/// Sets '1' if page acc->companies shows all companies and '0' if only
/// shows those in portfolio.
void conf_set_acc_all_cos (int all_cos);

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
// Copyright 10-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Management of 'servers&#46;db'

#ifndef IO_SERVERS_H
  #define IO_SERVERS_H

#include "dmc/async.h"
#include "data/Server.h"
#include "data/Rconf.h"

/*--*/

///
typedef struct servers_Servers Servers;

///
Js *servers_to_js(Servers *this);

///
Servers *servers_from_js(Js *js);

///
typedef struct servers_ServersIdNameCode ServersIdNameCode;

///
Js *serversIdNameCode_to_js(ServersIdNameCode *this);

/*--*/

///
void servers_init (void);

/// Arr[Server]
Arr *servers_list (void);

/// Returns Arr[Server] Historic active servers
Arr *servers_historic_list (void);

/// Returns Arr[Server] Daily active servers
Arr *servers_daily_list (void);

/// Returns the url of a company for acc->companies page (using infobolsa)
char *servers_acc_url (char *nick);

/// If 'short_name' is duplicate, operation is not done and it returns 0.
int servers_add (char *short_name);

/// Removes Server with id 'id'
void servers_remove (int id);

/// Sets name and short_name of Server with id 'id'. If short name is duplicated
/// returns 0. Otherwise returns 1.
int servers_set_names (int id, char *short_name, char *name);

/// Activates / Deactivates server
///   id: Server identifier. It it does not exists, this function does nothing.
///   historic: (1/0) If historic configuration will be updated
///   conf: Initial configuration
void servers_activate(int id, int historic, Rconf *conf);

/// Sets configurations.
///   id: Server identifier. It it does not exists, this function does nothing.
///   historic: (1/0) If historic configuration will be updated
///   conf: New configuration
void servers_set_conf(int id, int historic, Rconf *conf);

/// Set codes. If 'id' does not exists, this function does nothing.
///   id: Server id
///   codes: Arr[ServerCodes]. New codes
void servers_set_codes(int id, Arr *codes);

/// Adds a new nick with id 'nk_id' if it does not exist
void servers_add_nick (int nk_id);

/// Removes nick with id 'nk_id' if it exists.
void servers_del_nick (int nk_id);

/// Returns Arr[SeversIdNameCode]. If 'nick_id' does not have code, its code
/// value is an empty string.
Arr *servers_nick_codes (int nick_id);

/// Sets code of nick_id.
void servers_set_nick_code (int server_id, int nick_id, char *code);

/// Returns '1' if daily closes reading is correct. Otherwise returns '0' and
/// makes an annotation in Log.
int servers_test_daily_conf (int id);

/// Returns '1' if historic quotes reading is correct. Otherwise returns '0' and
/// makes an annotation in Log.
int servers_test_historic_conf (int id, int nk_id);

#endif
// Copyright 24-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Entry for historic data read from server.

#ifndef DATA_HISTORICENTRY_H
  #define DATA_HISTORICENTRY_H

#include "dmc/async.h"
#include "Quote.h"

/*--*/

///
///   Arguments:
///     date: char*
///     open: double
///     close: double
///     max: double
///     min: double
///     vol: int
typedef struct HistoricEntry_HistoricEntry HistoricEntry;

///
HistoricEntry *historicEntry_new (
  char *date,
  double open,
  double close,
  double max,
  double min,
  int vol
);

///
char *historicEntry_date (HistoricEntry *this);

///
double historicEntry_open (HistoricEntry *this);

///
double historicEntry_close (HistoricEntry *this);

///
double historicEntry_max (HistoricEntry *this);

///
double historicEntry_min (HistoricEntry *this);

///
int historicEntry_vol (HistoricEntry *this);

/*--*/

Quote *historicEntry_to_quote (HistoricEntry *this);

#endif
// Copyright 13-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Result records.

#ifndef DATA_RS_H
  #define DATA_RS_H

#include "dmc/async.h"
#include "Flea.h"
#include "Order.h"

/*--*/

///
///   Arguments:
///     assets: double
///     buys: int
///     sells: int
typedef struct Rs_RsAssets RsAssets;

///
RsAssets *rsAssets_new (double assets, int buys, int sells);

///
double rsAssets_assets (RsAssets *this);

///
int rsAssets_buys (RsAssets *this);

///
int rsAssets_sells (RsAssets *this);

///
Js *rsAssets_to_js (RsAssets *this);

///
RsAssets *rsAssets_from_js (Js *js);

///
///   Arguments:
///     avg: double
///     var: double
///     sel: double
typedef struct Rs_RsProfits RsProfits;

///
RsProfits *rsProfits_new (double avg, double var, double sel);

///
double rsProfits_avg (RsProfits *this);

///
double rsProfits_var (RsProfits *this);

///
double rsProfits_sel (RsProfits *this);

///
Js *rsProfits_to_js (RsProfits *this);

///
RsProfits *rsProfits_from_js (Js *js);

///
///   Arguments:
///     flea: Flea
///     assets: RsAssets
///     profits: RsProfits
typedef struct Rs_Rs Rs;

///
Rs *rs_new (Flea *flea, RsAssets *assets, RsProfits *profits);

///
Flea *rs_flea (Rs *this);

///
RsAssets *rs_assets (Rs *this);

///
RsProfits *rs_profits (Rs *this);

///
Js *rs_to_js (Rs *this);

///
Rs *rs_from_js (Js *js);

///
///   Arguments:
///     result: Rs
///     params: Darr
typedef struct Rs_RsWeb RsWeb;

///
RsWeb *rsWeb_new (Rs *result, Darr *params);

///
Rs *rsWeb_result (RsWeb *this);

///
Darr *rsWeb_params (RsWeb *this);

///
Js *rsWeb_to_js (RsWeb *this);

///
RsWeb *rsWeb_from_js (Js *js);

/// Bests fleas result
///   Arguments:
///     date: char*
///     result: RsWeb
typedef struct Rs_RsBests RsBests;

///
RsBests *rsBests_new (char *date, RsWeb *result);

///
char *rsBests_date (RsBests *this);

///
RsWeb *rsBests_result (RsBests *this);

///
Js *rsBests_to_js (RsBests *this);

///
RsBests *rsBests_from_js (Js *js);

/// Champion fleas result
///   Arguments:
///     model: char*
///     result: RsWeb
typedef struct Rs_RsChampions RsChampions;

///
RsChampions *rsChampions_new (char *model, RsWeb *result);

///
char *rsChampions_model (RsChampions *this);

///
RsWeb *rsChampions_result (RsChampions *this);

///
Js *rsChampions_to_js (RsChampions *this);

///
RsChampions *rsChampions_from_js (Js *js);

///
///   Arguments:
///     date: char*
///     close: double
///     ref: double
typedef struct Rs_RsChartQ RsChartQ;

///
RsChartQ *rsChartQ_new (char *date, double close, double ref);

///
char *rsChartQ_date (RsChartQ *this);

///
double rsChartQ_close (RsChartQ *this);

///
double rsChartQ_ref (RsChartQ *this);

///
Js *rsChartQ_to_js (RsChartQ *this);

///
RsChartQ *rsChartQ_from_js (Js *js);

///
///   Arguments:
///     is_sell: bool
///     date: char*
///     stocks: int
///     price: double
typedef struct Rs_RsChartOp RsChartOp;

///
RsChartOp *rsChartOp_new (
  int is_sell,
  char *date,
  int stocks,
  double price
);

///
int rsChartOp_is_sell (RsChartOp *this);

///
char *rsChartOp_date (RsChartOp *this);

///
int rsChartOp_stocks (RsChartOp *this);

///
double rsChartOp_price (RsChartOp *this);

///
Js *rsChartOp_to_js (RsChartOp *this);

///
RsChartOp *rsChartOp_from_js (Js *js);

///
///   Arguments:
///     nick: char*
///     profits: double
///     quotes: Arr-RsChartQ
///     historic: Arr-RsChartOp
typedef struct Rs_RsChart RsChart;

///
RsChart *rsChart_new (
  char *nick,
  double profits,
  Arr *quotes,
  Arr *historic
);

///
char *rsChart_nick (RsChart *this);

///
double rsChart_profits (RsChart *this);

/// Arr[RsChartQ] Dates and quotes
Arr *rsChart_quotes (RsChart *this);

/// Arr[RsChartOp] Operations
Arr *rsChart_historic (RsChart *this);

///
Js *rsChart_to_js (RsChart *this);

///
RsChart *rsChart_from_js (Js *js);

/// Charts fleas results
///   Arguments:
///     cos: Arr-RsChart
typedef struct Rs_RsCharts RsCharts;

///
RsCharts *rsCharts_new (Arr *cos);

/// Arr[RsChart]
Arr *rsCharts_cos (RsCharts *this);

/// Accounting results of one company
///   Arguments:
///     co: void
///     profits: double
///     quotes: Arr-RsChartQ
///     historic: Arr-RsChartOp
///     order: Order
///     ref: double
///     stocks: int
typedef struct Rs_RsHistoric RsHistoric;

///
RsHistoric *rsHistoric_new (
  void *co,
  double profits,
  Arr *quotes,
  Arr *historic,
  Order *order,
  double ref,
  int stocks
);

/// Company data
void *rsHistoric_co (RsHistoric *this);

///
double rsHistoric_profits (RsHistoric *this);

/// Arr[RsChartQ] Dates and quotes
Arr *rsHistoric_quotes (RsHistoric *this);

/// Arr[RsChartOp] Operations
Arr *rsHistoric_historic (RsHistoric *this);

/// Last order
Order *rsHistoric_order (RsHistoric *this);

/// Last ref
double rsHistoric_ref (RsHistoric *this);

/// stocks in portfolio
int rsHistoric_stocks (RsHistoric *this);

/*--*/

/// Returns 'bests' without duplicates filtering with flea (date + cycle + id)
///   bests: Arr[RsBests]
///   return: Arr[RsBests] without duplicates.
Arr *rsBests_distinct (Arr *bests);

#endif
// Copyright 10-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Server configuration (for daily and historic configurations)

#ifndef DATA_RCONF_H
  #define DATA_RCONF_H

#include "dmc/async.h"

/*--*/

///
///   Arguments:
///     url: char*
///     sel: int
///     is_date_eu: bool
///     date_separator: char*
///     is_iso_number: bool
///     fields_type: char*
///     table_start: char*
///     table_end: char*
///     row_start: char*
///     row_end: char*
///     cols_start: Arr-char*
///     cols_end: Arr-char*
typedef struct Rconf_Rconf Rconf;

///
char *rconf_url (Rconf *this);

/// enum Server
int rconf_sel (Rconf *this);

///
void rconf_set_sel (Rconf *this, int value);

///
int rconf_is_date_eu (Rconf *this);

///
char *rconf_date_separator (Rconf *this);

///
int rconf_is_iso_number (Rconf *this);

///
char *rconf_fields_type (Rconf *this);

///
char *rconf_table_start (Rconf *this);

///
char *rconf_table_end (Rconf *this);

///
char *rconf_row_start (Rconf *this);

///
char *rconf_row_end (Rconf *this);

///
Arr *rconf_cols_start (Rconf *this);

///
Arr *rconf_cols_end (Rconf *this);

///
Js *rconf_to_js (Rconf *this);

///
Rconf *rconf_from_js (Js *js);

/*--*/

#endif
// Copyright 23-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Accounting types.

#ifndef DATA_ACC_H
  #define DATA_ACC_H

#include "dmc/async.h"
#include "dmc/Darr.h"

/// Arr[AccPdEntry]
typedef Arr AccPf;

/*--*/

/// Data from Sell-Buy accounting entry
///   Arguments:
///     nick: char*
///     stocks: int
///     price: double
typedef struct Acc_AccSeBu AccSeBu;

///
char *accSeBu_nick (AccSeBu *this);

///
int accSeBu_stocks (AccSeBu *this);

///
double accSeBu_price (AccSeBu *this);

///
Js *accSeBu_to_js (AccSeBu *this);

///
AccSeBu *accSeBu_from_js (Js *js);

/// Data from Incomes-Withdraws accounting entry
///   Arguments:
///     ammount: double
typedef struct Acc_AccInWi AccInWi;

///
double accInWi_ammount (AccInWi *this);

///
Js *accInWi_to_js (AccInWi *this);

///
AccInWi *accInWi_from_js (Js *js);

/// Data from Profits-Fees-Differences accounting entry
///   Arguments:
///     ammount: double
///     cause: char*
typedef struct Acc_AccPrFePdNd AccPrFePdNd;

///
double accPrFePdNd_ammount (AccPrFePdNd *this);

///
char *accPrFePdNd_cause (AccPrFePdNd *this);

///
Js *accPrFePdNd_to_js (AccPrFePdNd *this);

///
AccPrFePdNd *accPrFePdNd_from_js (Js *js);

/// Accounting entry
///   Arguments:
///     operation: char*
///     id: int
///     date: char*
///     values: void
typedef struct Acc_AccEntry AccEntry;

/// one of "se", "bu", "in", "wi", "pr", "fe", "pd", "nd" or "cl"
char *accEntry_operation (AccEntry *this);

/// Entry identifier
int accEntry_id (AccEntry *this);

///
void accEntry_set_id (AccEntry *this, int value);

///
char *accEntry_date (AccEntry *this);

/// Historic profits
///   Arguments:
///     real: double
///     acc: double
///     risk: double
typedef struct Acc_AccHProfits AccHProfits;

///
double accHProfits_real (AccHProfits *this);

///
double accHProfits_acc (AccHProfits *this);

///
double accHProfits_risk (AccHProfits *this);

///
Js *accHProfits_to_js (AccHProfits *this);

///
AccHProfits *accHProfits_from_js (Js *js);

/// Historic entry
///   Arguments:
///     date: char*
///     profits: AccHProfits
typedef struct Acc_AccHEntry AccHEntry;

///
char *accHEntry_date (AccHEntry *this);

///
AccHProfits *accHEntry_profits (AccHEntry *this);

///
Js *accHEntry_to_js (AccHEntry *this);

///
AccHEntry *accHEntry_from_js (Js *js);

/// Acounting ledger
///   Arguments:
///     stocks: double
///     cash: double
///     capital: double
///     sells: double
///     fees: double
///     profits: double
///     differences: double
typedef struct Acc_AccLedger AccLedger;

///
double accLedger_stocks (AccLedger *this);

///
double accLedger_cash (AccLedger *this);

///
double accLedger_capital (AccLedger *this);

///
double accLedger_sells (AccLedger *this);

///
double accLedger_fees (AccLedger *this);

///
double accLedger_profits (AccLedger *this);

///
double accLedger_differences (AccLedger *this);

///
Js *accLedger_to_js (AccLedger *this);

/// Portfolio entry
///   Arguments:
///     nick: char*
///     stocks: int
///     price: double
///     quote: double
///     ref: double
typedef struct Acc_AccPfEntry AccPfEntry;

///
char *accPfEntry_nick (AccPfEntry *this);

///
int accPfEntry_stocks (AccPfEntry *this);

///
double accPfEntry_price (AccPfEntry *this);

///
double accPfEntry_quote (AccPfEntry *this);

///
void accPfEntry_set_quote (AccPfEntry *this, double value);

///
double accPfEntry_ref (AccPfEntry *this);

///
void accPfEntry_set_ref (AccPfEntry *this, double value);

///
Js *accPfEntry_to_js (AccPfEntry *this);

/// Tuple (errors (Arr[char]), ledger, pf)
///   Arguments:
///     errors: Arr-char*
///     ledger: AccLedger
///     pf: AccPf
typedef struct Acc_AccLedPf AccLedPf;

/// Arr[char]
Arr *accLedPf_errors (AccLedPf *this);

///
AccLedger *accLedPf_ledger (AccLedPf *this);

///
AccPf *accLedPf_pf (AccLedPf *this);

/*--*/

/// Constructor for "se", "bu"
AccEntry *accEntry_new1 (
  char *operation,
  int id,
  char *date,
  char *nick,
  int stocks,
  double price
);

/// Constructor for "in", "wi"
AccEntry *accEntry_new2 (
  char *operation,
  int id,
  char *date,
  double ammount
);

/// Constructor for "pr", "fe", "pd", "nd"
AccEntry *accEntry_new3 (
  char *operation,
  int id,
  char *date,
  double ammount,
  char *cause
);

/// Constructor for "cl"
AccEntry *accEntry_new4 (
  char *operation,
  int id,
  char *date
);

///
Js *accEntry_to_js(AccEntry *this);

///
AccEntry *accEntry_from_js(Js *js);

/// 'annotations' is Arr[AccEntry]
AccLedPf *accLedPf_new(Arr *annotations);

/// Returns profits [total, accounting, risk]
Darr *accLedPf_profits (AccLedPf *data);

#endif
// Copyright 11-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Market order.

#ifndef DATA_ORDER_H
  #define DATA_ORDER_H

#include "dmc/async.h"

/*--*/

///
///   Arguments:
///     type: int
///     ponderation: double
typedef struct Order_Order Order;

/// Returns ponderation of a buy order.
double order_ponderation (Order *this);

/*--*/

/// Creates a order to do nothing.
Order *order_none (void);

/// Creates a buy order. 'ponderation' must be greater than 0.
Order *order_buy (double ponderation);

/// Creates a sell order
Order *order_sell (void);

///
int order_is_none (Order *this);

///
int order_is_buy (Order *this);

///
int order_is_sell (Order *this);

///
typedef struct Order_OrderCos OrderCos;

/// Daily orders. Initialized to 0 orders.
///   ncos: Total number of companies.
OrderCos *orderCos_new(int ncos);

/// Returns an array with companies to buy, sorted by ponderations. Its size
/// 'orderCos_nbuys'
int *orderCos_buys(OrderCos *this);

/// Number of companies to buy and ponderations
int orderCos_nbuys(OrderCos *this);

/// Returns an array with companies to sell. Its size is 'orderCos_nsells'
int *orderCos_sells(OrderCos *this);

/// Number of companies to sell
int orderCos_nsells(OrderCos *this);

///
void orderCos_add(OrderCos *this, int co, Order *o);

#endif
// Copyright 14-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Flea accounting

#ifndef DATA_FACC_H
  #define DATA_FACC_H

#include "dmc/async.h"
#include "Qmatrix.h"

///
typedef struct Facc_Facc Facc;

/// Returns a new Acc with INTIAL_CAPITAL and without stocks.
///   ncos: Total companies number.
Facc *facc_new(int ncos);

///
double facc_cash(Facc *this);

/// Stocks in portfolio. Each index is a company. Thus acc_pf()[2] returns the
/// number of stocks of company "2".
int *facc_pf(Facc *this);

/// Buys stocks of a company.
///   co: Company
///   price: Price of each stock
///   returns: 1 if operation could be done or 0 otherwise.
int facc_buy(Facc *this, int co, double price);

/// Sells stocks of a company. Every stock will be sold. If the stocks number
/// is '0' it does nothing.
///   co: Company
///   price: Price of each stock
///   returns: 1 if operation could be done (stocks != 0) or 0 otherwise.
int facc_sell(Facc *this, int co, double price);

/// Returns total of assets discounting fees.
///   ncos: Companies number. Its value is equals to the size of 'closes' and
///         'facc_pf()'.
///   closes: Market closes. Each row is a date and in each row each index is
///           a company. Thus closes[0][2] is the close of company "2" in the
///           first date.
///   ix: Index of date (row) to calculate assets
double facc_assets(
  Facc *this,
  int ncos,
  Qmatrix *closes,
  int ix
);

#endif
// Copyright 11-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Interface for flea models.

#ifndef DATA_MODEL_H
  #define DATA_MODEL_H

#include "dmc/async.h"
#include "dmc/Darr.h"
#include "Flea.h"
#include "Order.h"
#include "Rs.h"
#include "Qmatrix.h"
#include "ModelMxMn.h"

/// Function which returns parameters of 'f'
typedef Darr *(*Fparams)(Flea *f);

/// Function which creates companies data.
///   params: Parameters (returned by 'model_params')
///   qnicks: Number of nicks-companies (columns of closes)
///   closes: Array of HISTORIC_QUOTES QmatrixValues (on for each date).
///   return: Arr[Co]. 'Co' changes for every model.
typedef Arr *(*Fcos)(Darr *params, int qnicks, QmatrixValues *closes);

/// Function which returns a market order.
///   params: Parameters (returned by model_params)
///   co: Company data. It is one of array elements returned by 'model_cos'
///       and 'Fcos'. It will be modified.
///   q: Close quote.
typedef Order *(*Forder)(Darr *params, void *co, double q);

/// Function wich returns actual current reference for calculating orders.
///   params: Parameters (returned by model_params)
///   co: Company data. It is one of elements returned by 'model_cos' and
///       'Fcos'.
typedef double (*Fref)(Darr *params, void *co);

///
///   Arguments:
///     name: char*
///     param_cf: Arr-ModelMxMn
///     param_jss: Js
///     fparams: Fparams
///     fcos: Fcos
///     forder: Forder
///     fref: Fref
typedef struct Model_Model Model;

///
Model *model_new (
  char *name,
  Arr *param_cf,
  Js *param_jss,
  Fparams fparams,
  Fcos fcos,
  Forder forder,
  Fref fref
);

/// Model name.
char *model_name (Model *this);

/// Arr[ModelMxMn] Names of model parameters.
Arr *model_param_cf (Model *this);

/// Template to show params in javascript (prefix-multiplicator-decimals-suffix)
Js *model_param_jss (Model *this);

/// Returns parameters of 'f'.
Darr *model_params(Model *this, Flea *f);

/// Creates companies data.
///   this: Model
///   params: Parameters (returned by 'model_params')
///   qnicks: Number of nicks-companies (columns of closes)
///   closes: Array of HISTORIC_QUOTES QmatrixValues (on for each date).
///   return: Arr[Co]. 'Co' changes for every model.
Arr *model_cos(Model *this, Darr *params, int qnicks, QmatrixValues *closes);

/// Returns a market order.
///   this: Model
///   params: Parameters (returned by model_params)
///   co: Company data. It is one of array elements returned by 'model_cos'
///       and 'Fcos'. It will be modified.
///   q: Close quote.
Order *model_order(Model *this, Darr *params, void *co, double q);

/// Returns actual current reference for calculating orders.
///   this: Model
///   params: Parameters (returned by model_params)
///   co: Company data. It is one of elements returned by 'model_cos' and
///       'Fcos'.
double model_ref(Model *this, Darr *params, void *co);

/// Calculates flea assets
RsAssets *model_assets(
  Model *this, Flea *f, Qmatrix *opens, Qmatrix *closes
);

/// Returns Arr[RankAssets] assets for a model-params.
Arr *model_assets_historic(
  Model *this, Darr *params, Arr *dates, Qmatrix *opens, Qmatrix *closes
);

/// Calculates flea profits
RsProfits *model_profits(
  Model *this, Flea *f, Qmatrix *opens, Qmatrix *closes
);

/// Calculates data for charts. 'dates' is Arr[char]
RsCharts *model_charts(
  Model *this, Flea *f, Arr *dates, Qmatrix *opens, Qmatrix *closes
);

/// Calculates historic results for one company
RsHistoric *model_historic (
  Model *this, Darr *params, Arr *dates, Darr *opens, Darr *closes
);

#endif
// Copyright 08-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Date and time table of holidays and special days.

#ifndef DATA_MARKETDAY_H
  #define DATA_MARKETDAY_H

#include "dmc/async.h"

/*--*/

///
///   Arguments:
///     date: char*
///     hopen: int
///     mopen: int
///     hclose: int
///     mclose: int
typedef struct MarketDay_MarketDay MarketDay;

///
char *marketDay_date (MarketDay *this);

///
int marketDay_hopen (MarketDay *this);

///
int marketDay_mopen (MarketDay *this);

///
int marketDay_hclose (MarketDay *this);

///
int marketDay_mclose (MarketDay *this);

///
Js *marketDay_to_js (MarketDay *this);

///
MarketDay *marketDay_from_js (Js *js);

/*--*/

#endif
// Copyright 13-Jul-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Tuple Name-Maximum-Minimum of a parameter.

#ifndef DATA_MODELMXMN_H
  #define DATA_MODELMXMN_H

#include "dmc/async.h"

/*--*/

/// Tuple Name-Maximum-Minimum of a parameter.
///   Arguments:
///     name: char*
///     max: double
///     min: double
typedef struct ModelMxMn_ModelMxMn ModelMxMn;

///
ModelMxMn *modelMxMn_new (char *name, double max, double min);

///
char *modelMxMn_name (ModelMxMn *this);

///
double modelMxMn_max (ModelMxMn *this);

///
double modelMxMn_min (ModelMxMn *this);

///
Js *modelMxMn_to_js (ModelMxMn *this);

///
ModelMxMn *modelMxMn_from_js (Js *js);

/*--*/

/// Returns Arr[char] Names of parameters. 'pmxmns' is Arr[ModelMxMn]
Arr *modelMxMn_names (Arr *pmxmns);

#endif
// Copyright 12-Jul-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Mananger who matches nick-model.

#ifndef DATA_MANAGER_H
  #define DATA_MANAGER_H

#include "dmc/async.h"
#include "ModelParams.h"

/*--*/

///
///   Arguments:
///     model: char*
///     params: Darr
typedef struct Manager_ManagerEntry ManagerEntry;

///
ManagerEntry *managerEntry_new (char *model, Darr *params);

///
char *managerEntry_model (ManagerEntry *this);

///
Darr *managerEntry_params (ManagerEntry *this);

///
Js *managerEntry_to_js (ManagerEntry *this);

///
ManagerEntry *managerEntry_from_js (Js *js);

///
///   Arguments:
///     current: ManagerEntry
///     entries: Map-ManagerEntry
typedef struct Manager_Manager Manager;

///
Manager *manager_new (ManagerEntry *current, Map *entries);

/// Default ManagerEntry
ManagerEntry *manager_current (Manager *this);

///
void manager_set_current (Manager *this, ManagerEntry *value);

/// Map[ManagerEntry]. Keys are nicks.
Map *manager_entries (Manager *this);

///
Js *manager_to_js (Manager *this);

///
Manager *manager_from_js (Js *js);

/// Format to show parameters in javascript
///   Arguments:
///     prefix: char*
///     multiplicator: int
///     decimals: int
///     suffix: char*
typedef struct Manager_ManagerFormat ManagerFormat;

///
ManagerFormat *managerFormat_new (
  char *prefix,
  int multiplicator,
  int decimals,
  char *suffix
);

///
char *managerFormat_prefix (ManagerFormat *this);

///
int managerFormat_multiplicator (ManagerFormat *this);

///
int managerFormat_decimals (ManagerFormat *this);

///
char *managerFormat_suffix (ManagerFormat *this);

///
Js *managerFormat_to_js (ManagerFormat *this);

///
ManagerFormat *managerFormat_from_js (Js *js);

///
///   Arguments:
///     model: char*
///     params: Darr
///     param_cf: Arr-ModelMxMn
///     param_fmt: Arr-ManagerFormat
typedef struct Manager_ManagerEntry2 ManagerEntry2;

///
ManagerEntry2 *managerEntry2_new (
  char *model,
  Darr *params,
  Arr *param_cf,
  Arr *param_fmt
);

///
char *managerEntry2_model (ManagerEntry2 *this);

///
Darr *managerEntry2_params (ManagerEntry2 *this);

///
Arr *managerEntry2_param_cf (ManagerEntry2 *this);

///
Arr *managerEntry2_param_fmt (ManagerEntry2 *this);

///
Js *managerEntry2_to_js (ManagerEntry2 *this);

///
///   Arguments:
///     current: ManagerEntry2
///     entries: Map-ManagerEntry2
typedef struct Manager_Manager2 Manager2;

///
Manager2 *manager2_new (ManagerEntry2 *current, Map *entries);

/// Default ManagerEntry
ManagerEntry2 *manager2_current (Manager2 *this);

/// Map[ManagerEntry2]. Keys are nicks.
Map *manager2_entries (Manager2 *this);

///
Js *manager2_to_js (Manager2 *this);

/*--*/

#endif
// Copyright 03-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Error message.

#ifndef DATA_EMSG_H
  #define DATA_EMSG_H

#include "dmc/async.h"

/*--*/

///
///   Arguments:
///     error: int
///     msg: char*
typedef struct EMsg_EMsg EMsg;

///
EMsg *eMsg_new (int error, char *msg);

/// Its value is one of ErrorMsg defined in DEFS.h
int eMsg_error (EMsg *this);

///
char *eMsg_msg (EMsg *this);

/*--*/

#endif
// Copyright 24-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Tuple model-parameters of the model used in accounting.

#ifndef DATA_MODELPARAMS_H
  #define DATA_MODELPARAMS_H

#include "dmc/async.h"
#include "dmc/Darr.h"
#include "Model.h"

/*--*/

///
///   Arguments:
///     model: Model
///     params: Darr
typedef struct ModelParams_ModelParams ModelParams;

///
ModelParams *modelParams_new (Model *model, Darr *params);

///
Model *modelParams_model (ModelParams *this);

///
Darr *modelParams_params (ModelParams *this);

/*--*/

/// Returns 1 if model names are equals and params are equals with a gap of
/// 0.0000001
int modelParams_eq (ModelParams *p1, ModelParams *p2);
#endif
// Copyright 16-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Broker operations.

#ifndef DATA_BROKER_H
  #define DATA_BROKER_H

#include "dmc/async.h"

/// Returns tatal fees of a buy or sell operation.
double broker_fees (double money);

/// Returns net cost of operation
double broker_buy (int stocks, double price);

/// Returns net incomes of operation
double broker_sell (int stocks, double price);

#endif
// Copyright 02-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// MM2B model.

#ifndef DATA_DFLEAS_MM_MM__2B_H
  #define DATA_DFLEAS_MM_MM__2B_H

#include "dmc/async.h"
#include "data/Model.h"

///
Model *mm__2B (void);

#endif
// Copyright 02-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Base for MM models.

#ifndef DATA_DFLEAS_MM_MMBASE_H
  #define DATA_DFLEAS_MM_MMBASE_H

#include "dmc/async.h"
#include "data/Model.h"
#include "dmc/Darr.h"

///
typedef struct mMBase_MMBase MMBase;

struct mMBase_MMBase {
  int to_sell;
  double ref;
  double mm;
};

///
MMBase *mMBase_new(int to_sell, double ref, double q);

/// Returns Arr[MMBase]
Arr *mMBase_cos (int qnicks, QmatrixValues *closes, double strip_to_sell);

///
Order *mMBase_order (
  MMBase *this, double q,
  double strip_to_buy, double step_to_buy,
  double strip_to_sell, double step_to_sell
);

#endif
// Copyright 02-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// MM4 model.

#ifndef DATA_DFLEAS_MM_MM__4_H
  #define DATA_DFLEAS_MM_MM__4_H

#include "dmc/async.h"
#include "data/Model.h"

///
Model *mm__4 (void);

#endif
// Copyright 02-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// MM1 model.

#ifndef DATA_DFLEAS_MM_MM__1_H
  #define DATA_DFLEAS_MM_MM__1_H

#include "dmc/async.h"
#include "data/Model.h"

///
Model *mm__1 (void);

#endif
// Copyright 02-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// MM2A model.

#ifndef DATA_DFLEAS_MM_MM__2A_H
  #define DATA_DFLEAS_MM_MM__2A_H

#include "dmc/async.h"
#include "data/Model.h"

///
Model *mm__2A (void);

#endif
// Copyright 15-Dec-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#ifndef DATA_DFLEAS_DFLEAS__INCRMM_H
  #define DATA_DFLEAS_DFLEAS__INCRMM_H

#include "dmc/async.h"

/// Returns Arr[Model]
Arr *dfleas__IncrMM_models (void);

#endif
// Copyright 02-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// MMBack models.<p>
/// Its main difference with MM models is that when last operation is a lost
/// start at the maximum-minimum of two cycles (MM models start at last
/// cycle ever)

#ifndef DATA_DFLEAS_DFLEAS__MMBACK_H
  #define DATA_DFLEAS_DFLEAS__MMBACK_H

#include "dmc/async.h"
#include "data/Model.h"

/// Returns Arr[Model]
Arr *dfleas__MMBack_models (void);

/// Returns default model
Model *dfleas__MMBack_default (void);

#endif
// Copyright 02-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// GA3 model.

#ifndef DATA_DFLEAS_GA_GA__3_H
  #define DATA_DFLEAS_GA_GA__3_H

#include "dmc/async.h"
#include "data/Model.h"

///
Model *ga__3 (void);

#endif
// Copyright 02-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Base for GA models

#ifndef DATA_DFLEAS_GA_GABASE_H
  #define DATA_DFLEAS_GA_GABASE_H

#include "dmc/async.h"
#include "data/Model.h"
#include "dmc/Darr.h"

///
typedef struct gABase_GABase GABase;

struct gABase_GABase {
  int to_sell;
  double sum;
  double num;
  QmatrixValues *closes;
  int con; // Company number
  int closes_ix; // Index (day) of Qmatrix
  int days_ix;
  double dv;
  double ref;
};

///
GABase *gABase_new(QmatrixValues *closes, int con, int days);

/// Returns Arr[IncrBase]
Arr *gABase_cos (int qnicks, QmatrixValues *closes, int days);

///
Order *gABase_order (
  GABase *this, double q, int days, double strip_to_buy, double strip_to_sell
);

///
double gABase_ref (
  GABase *this, double strip_to_buy, double strip_to_sell
);

#endif
// Copyright 01-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#ifndef DATA_DFLEAS_DFLEAS__FMT_H
  #define DATA_DFLEAS_DFLEAS__FMT_H

/// Format functions to model

#include "dmc/async.h"

/// Js format for days.
Js *dfleas__fmt_day (void);

/// Js format for Percentages.
Js *dfleas__fmt_perc (void);

#endif
// Copyright 02-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// MA models.

#ifndef DATA_DFLEAS_DFLEAS__MA_H
  #define DATA_DFLEAS_DFLEAS__MA_H

#include "dmc/async.h"

/// Returns Arr[Model]
Arr *dfleas__MA_models (void);

#endif
// Copyright 01-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Base for Incr models.

#ifndef DATA_DFLEAS_INCR_INCRBASE_H
  #define DATA_DFLEAS_INCR_INCRBASE_H

#include "dmc/async.h"
#include "data/Model.h"
#include "dmc/Darr.h"

///
typedef struct incrBase_IncrBase IncrBase;

///
struct incrBase_IncrBase {
  int to_sell;
  QmatrixValues *closes;
  double ref;
  int con; // Company number
  int closes_ix; // Index (day) of Qmatrix
  int days_ix;
};

///
IncrBase *incrBase_new(
  int to_sell, QmatrixValues *closes, double ref, int con
);

/// Returns Arr[IncrBase]
Arr *incrBase_cos (int qnicks, QmatrixValues *closes, double strip_to_sell);

///
Order *incrBase_order (
  IncrBase *this, double q, int days, double strip_to_buy, double strip_to_sell
);

///
double incrBase_ref (
  IncrBase *this, double strip_to_buy, double strip_to_sell
);

#endif
// Copyright 01-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Incr3 model

#ifndef DATA_DFLEAS_INCR_INCR__3_H
  #define DATA_DFLEAS_INCR_INCR__3_H

#include "dmc/async.h"
#include "data/Model.h"

///
Model *incr__3 (void);

#endif
// Copyright 01-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Incr2 model

#ifndef DATA_DFLEAS_INCR_INCR__2_H
  #define DATA_DFLEAS_INCR_INCR__2_H

#include "dmc/async.h"
#include "data/Model.h"

///
Model *incr__2 (void);

#endif
// Copyright 14-Dec-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#ifndef DATA_DFLEAS_MMBACK_MMBACK__4_H
  #define DATA_DFLEAS_MMBACK_MMBACK__4_H

#include "dmc/async.h"
#include "data/Model.h"

///
Model *mmBack__4 (void);

#endif
// Copyright 02-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// MMBack1 model.

#ifndef DATA_DFLEAS_MMBACK_MMBACK__1_H
  #define DATA_DFLEAS_MMBACK_MMBACK__1_H

#include "dmc/async.h"
#include "data/Model.h"

///
Model *mmBack__1 (void);

#endif
// Copyright 02-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// MMBack2 model.

#ifndef DATA_DFLEAS_MMBACK_MMBACK__2_H
  #define DATA_DFLEAS_MMBACK_MMBACK__2_H

#include "dmc/async.h"
#include "data/Model.h"

///
Model *mmBack__2 (void);

#endif
// Copyright 02-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Base for MMBack models.

#ifndef DATA_DFLEAS_MMBACK_MMBACKBASE_H
  #define DATA_DFLEAS_MMBACK_MMBACKBASE_H

#include "dmc/async.h"
#include "data/Model.h"
#include "dmc/Darr.h"

///
typedef struct mMBackBase_MMBackBase MMBackBase;

struct mMBackBase_MMBackBase {
  int to_sell;
  double ref;
  double mm;
  double pmm1;
  double pmm2;
  double q;
};

///
MMBackBase *mMBackBase_new(int to_sell, double ref, double q);

/// Returns Arr[MMBase]
Arr *mMBackBase_cos (int qnicks, QmatrixValues *closes, double strip_to_sell);

///
Order *mMBackBase_order (
  MMBackBase *this, double q,
  double strip_to_buy, double step_to_buy,
  double strip_to_sell, double step_to_sell
);

#endif
// Copyright 02-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Base for MMWin models.

#ifndef DATA_DFLEAS_MMWIN_MMWINBASE_H
  #define DATA_DFLEAS_MMWIN_MMWINBASE_H

#include "dmc/async.h"
#include "data/Model.h"
#include "dmc/Darr.h"

///
typedef struct mMWinBase_MMWinBase MMWinBase;

struct mMWinBase_MMWinBase {
  int to_sell;
  double ref;
  double mm;
  double ref0; // mm of day of last operation
  double qop; // close of day of last operation
  double qlast; // close of last day
};

///
MMWinBase *mMWinBase_new(int to_sell, double ref, double max);

/// Returns Arr[MMBase]
Arr *mMWinBase_cos (int qnicks, QmatrixValues *closes, double strip_to_sell);

///
Order *mMWinBase_order (
  MMWinBase *this, double q,
  double strip_to_buy, double step_to_buy,
  double strip_to_sell, double step_to_sell
);

///
double mMWinBase_ref (MMWinBase *this);

#endif
// Copyright 02-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// MMWin1 model.

#ifndef DATA_DFLEAS_MMWIN_MMWIN__1_H
  #define DATA_DFLEAS_MMWIN_MMWIN__1_H

#include "dmc/async.h"
#include "data/Model.h"

///
Model *mmWin__1 (void);

#endif
// Copyright 02-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// MMWin2 model.

#ifndef DATA_DFLEAS_MMWIN_MMWIN__2_H
  #define DATA_DFLEAS_MMWIN_MMWIN__2_H

#include "dmc/async.h"
#include "data/Model.h"

///
Model *mmWin__2 (void);

#endif
// Copyright 14-Dec-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#ifndef DATA_DFLEAS_MMWIN_MMWIN__4_H
  #define DATA_DFLEAS_MMWIN_MMWIN__4_H

#include "dmc/async.h"
#include "data/Model.h"

///
Model *mmWin__4 (void);

#endif
// Copyright 02-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// GA models.

#ifndef DATA_DFLEAS_DFLEAS__GA_H
  #define DATA_DFLEAS_DFLEAS__GA_H

#include "dmc/async.h"

/// Returns Arr[Model]
Arr *dfleas__GA_models (void);

#endif
// Copyright 02-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// MM models.<p>
/// The main difference with Approx models is that theese start at an
/// arbitrary percentual value, but MM models start at a previous maximum or
/// minimum.

#ifndef DATA_DFLEAS_DFLEAS__MM_H
  #define DATA_DFLEAS_DFLEAS__MM_H

#include "dmc/async.h"

/// Returns Arr[Model]
Arr *dfleas__MM_models (void);


#endif
// Copyright 02-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Approx2 model.

#ifndef DATA_DFLEAS_APPROX_APPROX__2_H
  #define DATA_DFLEAS_APPROX_APPROX__2_H

#include "dmc/async.h"
#include "data/Model.h"

///
Model *approx__2 (void);

#endif
// Copyright 02-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Approx3B model.

#ifndef DATA_DFLEAS_APPROX_APPROX__3B_H
  #define DATA_DFLEAS_APPROX_APPROX__3B_H

#include "dmc/async.h"
#include "data/Model.h"

///
Model *approx__3B (void);

#endif
// Copyright 02-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Approx3A model.

#ifndef DATA_DFLEAS_APPROX_APPROX__3A_H
  #define DATA_DFLEAS_APPROX_APPROX__3A_H

#include "dmc/async.h"
#include "data/Model.h"

///
Model *approx__3A (void);

#endif
// Copyright 02-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Approx4 model.

#ifndef DATA_DFLEAS_APPROX_APPROX__4_H
  #define DATA_DFLEAS_APPROX_APPROX__4_H

#include "dmc/async.h"
#include "data/Model.h"

///
Model *approx__4 (void);


#endif
// Copyright 02-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Base for Approx models.

#ifndef DATA_DFLEAS_APPROX_APPROXBASE_H
  #define DATA_DFLEAS_APPROX_APPROXBASE_H

#include "dmc/async.h"
#include "data/Model.h"
#include "dmc/Darr.h"

///
typedef struct approxBase_ApproxBase ApproxBase;

struct approxBase_ApproxBase {
  int to_sell;
  double ref;
};

///
ApproxBase *approxBase_new(int to_sell, double ref);

/// Returns Arr[ApproxBase]
Arr *approxBase_cos (int qnicks, QmatrixValues *closes, double start_to_sell);

///
Order *approxBase_order (
  ApproxBase *this, double q,
  double start_to_buy, double step_to_buy,
  double start_to_sell, double step_to_sell
);

#endif
// Copyright 02-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// MMBack models.<p>
/// Its main difference with MM models is that MMWin wait until winning or
/// breaking then initial support-resistence.

#ifndef DATA_DFLEAS_DFLEAS__MMWIN_H
  #define DATA_DFLEAS_DFLEAS__MMWIN_H

#include "dmc/async.h"
#include "data/Model.h"

/// Returns Arr[Model]
Arr *dfleas__MMWin_models (void);

#endif
// Copyright 15-Dec-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#ifndef DATA_DFLEAS_INCRMM_INCRMMBASE_H
  #define DATA_DFLEAS_INCRMM_INCRMMBASE_H

#include "dmc/async.h"
#include "data/Model.h"
#include "dmc/Darr.h"

///
typedef struct incrMMBase_IncrMMBase IncrMMBase;

///
struct incrMMBase_IncrMMBase {
  int to_sell;
  QmatrixValues *closes;
  double ref; // ref = closes[days_ix + day_mm_ix][con] +- strip
  int con; // Company number
  int closes_ix; // Index (day) of Qmatrix
  int days_ix;
  int day_mm_ix;
};

///
IncrMMBase *incrMMBase_new(
  int to_sell, QmatrixValues *closes, int con
);

/// Returns Arr[IncrBase]
Arr *incrMMBase_cos (int qnicks, QmatrixValues *closes, double strip_to_sell);

///
Order *incrMMBase_order (
  IncrMMBase *this, double q, int days,
  double strip_to_buy, double strip_to_sell
);

///
double incrMMBase_ref (
  IncrMMBase *this, double strip_to_buy, double strip_to_sell
);

#endif
// Copyright 15-Dec-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#ifndef DATA_DFLEAS_INCRMM_INCRMM__3_H
  #define DATA_DFLEAS_INCRMM_INCRMM__3_H

#include "dmc/async.h"
#include "data/Model.h"

///
Model *incrMM__3 (void);

#endif
// Copyright 01-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Incr models.

#ifndef DATA_DFLEAS_DFLEAS__INCR_H
  #define DATA_DFLEAS_DFLEAS__INCR_H

#include "dmc/async.h"

/// Returns Arr[Model]
Arr *dfleas__Incr_models (void);

#endif
// Copyright 02-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// MA3 model.

#ifndef DATA_DFLEAS_MA_MA__3_H
  #define DATA_DFLEAS_MA_MA__3_H

#include "dmc/async.h"
#include "data/Model.h"

///
Model *ma__3 (void);

#endif
// Copyright 02-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Base for MA models

#ifndef DATA_DFLEAS_MA_MABASE_H
  #define DATA_DFLEAS_MA_MABASE_H

#include "dmc/async.h"
#include "data/Model.h"
#include "dmc/Darr.h"

///
typedef struct mABase_MABase MABase;

struct mABase_MABase {
  int to_sell;
  double sum;
  QmatrixValues *closes;
  int con; // Company number
  int closes_ix; // Index (day) of Qmatrix
  int days_ix;
};

///
MABase *mABase_new(QmatrixValues *closes, int con, int days);

/// Returns Arr[IncrBase]
Arr *mABase_cos (int qnicks, QmatrixValues *closes, int days);

///
Order *mABase_order (
  MABase *this, double q, int days, double strip_to_buy, double strip_to_sell
);

///
double mABase_ref (
  MABase *this, double strip_to_buy, double strip_to_sell
);

#endif
// Copyright 02-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// MA2 model.

#ifndef DATA_DFLEAS_MA_MA__2_H
  #define DATA_DFLEAS_MA_MA__2_H

#include "dmc/async.h"
#include "data/Model.h"

///
Model *ma__2 (void);

#endif
// Copyright 12-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Fleas model generator.

#ifndef DATA_DFLEAS_FLEAS__MODELS_H
  #define DATA_DFLEAS_FLEAS__MODELS_H

#include "dmc/async.h"
#include "data/ModelParams.h"

///
void dfleas__models_init (void);

/// Arr[Model]
Arr *dfleas__models (void);

/// Arr[char]
Arr *dfleas__models_names (void);

/// Returns Opt[Model] The model called 'name'
Opt *dfleas__models_get (char *name);

/// Returns Model-Parameters used by default in program initialization.
ModelParams *dfleas__models_default (void);

#endif
// Copyright 02-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Approx models.

#ifndef DATA_DFLEAS_DFLEAS__APPROX_H
  #define DATA_DFLEAS_DFLEAS__APPROX_H

#include "dmc/async.h"

/// Returns Arr[Model]
Arr *dfleas__Approx_models (void);

#endif
// Copyright 28-Nov-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#ifndef DATA_RANKEVALENTRY_H
  #define DATA_RANKEVALENTRY_H

#include "dmc/async.h"
#include "Flea.h"
#include "Model.h"

/*--*/

///
///   Arguments:
///     model_name: char*
///     flea: Flea
///     assets: double
///     profits: double
///     days: double
///     points: double
typedef struct RankEvalEntry_RankEvalEntry RankEvalEntry;

///
RankEvalEntry *rankEvalEntry_new (
  char *model_name,
  Flea *flea,
  double assets,
  double profits,
  double days,
  double points
);

///
char *rankEvalEntry_model_name (RankEvalEntry *this);

///
Flea *rankEvalEntry_flea (RankEvalEntry *this);

///
double rankEvalEntry_assets (RankEvalEntry *this);

/// avg moduled with variance
double rankEvalEntry_profits (RankEvalEntry *this);

///
double rankEvalEntry_days (RankEvalEntry *this);

///
double rankEvalEntry_points (RankEvalEntry *this);

///
void rankEvalEntry_set_points (RankEvalEntry *this, double value);

///
Js *rankEvalEntry_to_js (RankEvalEntry *this);

///
RankEvalEntry *rankEvalEntry_from_js (Js *js);

/*--*/

/// Returns Opt<Model>
Opt *rankEvalEntry_model (RankEvalEntry *this);

#endif
// Copyright 24-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Entry for daily data read from server.

#ifndef DATA_DAILYENTRY_H
  #define DATA_DAILYENTRY_H

#include "dmc/async.h"

/*--*/

///
///   Arguments:
///     code: char*
///     close: double
typedef struct DailyEntry_DailyEntry DailyEntry;

///
DailyEntry *dailyEntry_new (char *code, double close);

///
char *dailyEntry_code (DailyEntry *this);

///
double dailyEntry_close (DailyEntry *this);

/*--*/

#endif
// Copyright 08-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Time table of market open-close.

#ifndef DATA_TIMETABLE_H
  #define DATA_TIMETABLE_H

#include "dmc/async.h"

/*--*/

///
///   Arguments:
///     hopen: int
///     mopen: int
///     hclose: int
///     mclose: int
typedef struct Timetable_Timetable Timetable;

///
int timetable_hopen (Timetable *this);

///
int timetable_mopen (Timetable *this);

///
int timetable_hclose (Timetable *this);

///
int timetable_mclose (Timetable *this);

///
Js *timetable_to_js (Timetable *this);

///
Timetable *timetable_from_js (Js *js);

/*--*/

/// Creates a time table with values: hopen = 0, mopen = 0, hclose = 23,
/// mclose = 55
Timetable *timetable_new(void);
#endif
// Copyright 28-Nov-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#ifndef DATA_RANKENTRY_H
  #define DATA_RANKENTRY_H

#include "dmc/async.h"
#include "Flea.h"
#include "Model.h"

/*--*/

///
///   Arguments:
///     model_name: char*
///     flea: Flea
typedef struct RankEntry_RankEntry RankEntry;

///
RankEntry *rankEntry_new (char *model_name, Flea *flea);

///
char *rankEntry_model_name (RankEntry *this);

///
Flea *rankEntry_flea (RankEntry *this);

///
Js *rankEntry_to_js (RankEntry *this);

///
RankEntry *rankEntry_from_js (Js *js);

/*--*/

/// Returns Opt<Model>
Opt *rankEntry_model (RankEntry *this);

#endif
// Copyright 28-Nov-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#ifndef DATA_RANKASSETSENTRY_H
  #define DATA_RANKASSETSENTRY_H

#include "dmc/async.h"
#include "Flea.h"
#include "Model.h"

/*--*/

///
///   Arguments:
///     model_name: char*
///     flea: Flea
///     assets: int
///     points: int
typedef struct RankAssetsEntry_RankAssetsEntry RankAssetsEntry;

///
RankAssetsEntry *rankAssetsEntry_new (
  char *model_name,
  Flea *flea,
  int assets,
  int points
);

///
char *rankAssetsEntry_model_name (RankAssetsEntry *this);

///
Flea *rankAssetsEntry_flea (RankAssetsEntry *this);

///
int rankAssetsEntry_assets (RankAssetsEntry *this);

///
int rankAssetsEntry_points (RankAssetsEntry *this);

///
Js *rankAssetsEntry_to_js (RankAssetsEntry *this);

///
RankAssetsEntry *rankAssetsEntry_from_js (Js *js);

/*--*/

/// Returns Opt<Model>
Opt *rankAssetsEntry_model (RankAssetsEntry *this);

#endif
// Copyright 11-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Flea gen.

#ifndef DATA_GEN_H
  #define DATA_GEN_H

#include "dmc/async.h"
#include "dmc/Darr.h"

///
typedef Darr Gen;

/// Creates a new gen with n values equals to 0.5.
Gen *gen_new(int n);

/// Number of elements of 'this'
int gen_n(Gen *this);

/// Pointer to values of 'this'
double *gen_values(Gen *this);

/// Returns a new gen mutation of 'this'
Gen *gen_mutate(Gen *this);

/// Returns a duplicate of 'this'
Gen *gen_copy(Gen *this);

///
Js *gen_to_js(Gen *this);

///
Gen *gen_from_js(Js *js);

#endif
// Copyright 06-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Nick data.

#ifndef DATA_NICK_H
  #define DATA_NICK_H

#include "dmc/async.h"

/*--*/

///
///   Arguments:
///     id: int
///     name: char*
///   Variables:
///     is_sel: bool
typedef struct Nick_Nick Nick;

///
Nick *nick_new (int id, char *name);

///
int nick_id (Nick *this);

///
char *nick_name (Nick *this);

///
void nick_set_name (Nick *this, char *value);

///
int nick_is_sel (Nick *this);

///
void nick_set_is_sel (Nick *this, int value);

///
Js *nick_to_js (Nick *this);

///
Nick *nick_from_js (Js *js);

/*--*/

#endif
// Copyright 11-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Quotes values (opens or closes) used by fleas.

#ifndef DATA_QMATRIX_H
  #define DATA_QMATRIX_H

#include "dmc/async.h"

/// Array of doubles
typedef double * QmatrixValues;

/*--*/

///
///   Arguments:
///     nicks: Arr-Nick
///     values: QmatrixValues
typedef struct Qmatrix_Qmatrix Qmatrix;

///
Qmatrix *qmatrix_new (Arr *nicks, QmatrixValues *values);

/// Arr[Nick]
Arr *qmatrix_nicks (Qmatrix *this);

/// Array of HISTORIC_QUOTES of QmatrixValues (one for each date). Every
/// QmatrixValues has 'nicks size' doubles.
QmatrixValues *qmatrix_values (Qmatrix *this);

/*--*/

/// Adds 'row' to 'table' and remove the first row of 'table'
void qmatrix_add(QmatrixValues *table, QmatrixValues row);

#endif
// Copyright 07-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Quote data and utilities.

#ifndef DATA_QUOTE_H
  #define DATA_QUOTE_H

#include "dmc/async.h"

/*--*/

///
///   Arguments:
///     date: char*
///     open: double
///     close: double
///     max: double
///     min: double
///     vol: int
///     error: bool
typedef struct Quote_Quote Quote;

///
Quote *quote_new (
  char *date,
  double open,
  double close,
  double max,
  double min,
  int vol,
  int error
);

///
char *quote_date (Quote *this);

///
double quote_open (Quote *this);

///
double quote_close (Quote *this);

///
double quote_max (Quote *this);

///
double quote_min (Quote *this);

///
int quote_vol (Quote *this);

/// It it is 1, quotes are handly modified.
int quote_error (Quote *this);

///
Js *quote_to_js (Quote *this);

///
Quote *quote_from_js (Js *js);

/*--*/

/// Returns Opt[Quote]
Opt *quote_from_str (char *q);

///
char *quote_to_str (Quote *q);

/// Unifies serveral arrays of quotes in one.
///   aqs: Arr[Arr[Quote]]] Arrays to unify
///   qs: Arr[Quote] One of 'aqs' selected for tiebreak
///   return Arr[Quote] A new array with the result.
Arr *quote_unify (Arr *aqs, Arr *best_qs);

/// Checks maximum and minimum and returns a new quote corrected.<p>
/// If quote_error(q) = 1, quote will not be corrected.<p>
/// If quote was corrected, its 'error' field is set to 1
///   q     : Quote to correct
///   return: Kv[Quote]
///           - If there was an error 'kv_key' is a message (e.g. Max > Min)
///             and 'kv_value' is a new quote equals to 'q' corrected.
///           - If there was no error 'kv_key' is a blank string and 'kv_value'
///             is a new quote equals to 'q'.
Kv *quote_corr1 (Quote *q);

/// Checks maximum and minimum and returns a new quote corrected.<p>
/// If quote_error(last) = 1, quote will not be corrected.<p>
/// If quote was corrected, its 'error' field is set to 1
///   last: Quote to correct
///   previous: Quote previous to 'last'
///   return: Kv[Quote]
///           - If there was an error 'kv_key' is a message (e.g. Max > Min)
///             and 'kv_value' is a new quote equals to 'q' corrected.
///           - If there was no error 'kv_key' is a blank string and 'kv_value'
///             is a new quote equals to 'q'.
Kv *quote_corr2 (Quote *last, Quote *previous);

/// Checks increment and returns a new quote corrected.<p>
/// If quote_error(last) = 1, quote will not be corrected.<p>
/// If quote has an incerment +-20%, its 'error' field is set to 1
///   last: Quote to correct
///   previous: Quote previous to 'last'
///   return: Kv[Quote]
///           - If there was an error 'kv_key' is a message (e.g. Max > Min)
///             and 'kv_value' is a new quote equals to 'q' corrected.
///           - If there was no error 'kv_key' is a blank string and 'kv_value'
///             is a new quote equals to 'q'.
Kv *quote_corr3 (Quote *last, Quote *previous);

/// Checks quotes which field 'error' is '= 0', in 'qs'.
///   qs: Arr[Quote] Quotes to check.
///   return: Tp of
///     Arr[char] Errors returned by 'corr1', 'corr2' and 'corr3' with format
///       "date: error". If there is no error, the array is empty.
///     Arr[Quote]. Array corrected.
Tp *quote_check (Arr *qs);

/// Checks dates of 'qs' matching them with the ones of 'model'.
///   model: Arr[Quote] Quotes of nick model.
///   qs: Arr[Quote] Quotes to check.
///   return: Tp of
///     Arr[char] Errors by extra or missing quotes. If there is no error,
///       the array is empty.
///     Arr[Quote]. Array corrected.
Tp *quote_check_dates (Arr *model, Arr *qs);

/// Blends new quotes with others already existent.<p>
/// All Arr are Arr[Quote]
///   model : Arr[Quote]. Model quotes. It can be empty.
///   new   : Arr[Quote]. Last quotes read from the Internet. It can be empty.
///   old   : Arr[Quote]. Existent quotes in file system. It can be empty.
///   return: Tp of
///     Arr[char] Errors returned by 'corr1' and 'corr2' with format
///       "date: error". If there is no error, the array is empty.
///     Arr[Quote]. Array made with the following process:
///        1. Every quote on top with 'open = -1' is removed from 'old' in the
///           dates range of 'new'
///        2. If there are new and old quotes for the same date, that of 'old'
///           is selected.
///        3. The return array is corrected in the range of 'new' dates and
///           adding or removing quotes maching model quotes.
Tp *quote_blend (Arr *model, Arr *new, Arr *old);

#endif
// Copyright 24-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Tuple nick-close used in data/Server and quotes_sets.

#ifndef DATA_NICKCLOSE_H
  #define DATA_NICKCLOSE_H

#include "dmc/async.h"

/*--*/

///
///   Arguments:
///     nick: int
///     close: double
typedef struct NickClose_NickClose NickClose;

///
NickClose *nickClose_new (int nick, double close);

/// Nick id
int nickClose_nick (NickClose *this);

///
double nickClose_close (NickClose *this);

/*--*/

#endif
// Copyright 07-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Data to manage rankings

#ifndef DATA_RANK_H
  #define DATA_RANK_H

#include "dmc/async.h"
#include "Rs.h"
#include "RankAssetsEntry.h"

/*--*/

/// Pair date-double to use with charts.
///   Arguments:
///     date: char*
///     assets: double
typedef struct Rank_RankAssets RankAssets;

///
RankAssets *rankAssets_new (char *date, double assets);

///
char *rankAssets_date (RankAssets *this);

///
double rankAssets_assets (RankAssets *this);

///
Js *rankAssets_to_js (RankAssets *this);

///
RankAssets *rankAssets_from_js (Js *js);

/// Pair date-int to use with charts.
///   Arguments:
///     date: char*
///     position: int
typedef struct Rank_RankPosition RankPosition;

///
RankPosition *rankPosition_new (char *date, int position);

///
char *rankPosition_date (RankPosition *this);

///
int rankPosition_position (RankPosition *this);

///
Js *rankPosition_to_js (RankPosition *this);

///
RankPosition *rankPosition_from_js (Js *js);

/// Data to send to client.
///   Arguments:
///     result: RsChampions
///     assets: Arr-RankAssets
///     positions: Arr-RankPosition
typedef struct Rank_RankFlea RankFlea;

///
RankFlea *rankFlea_new (RsChampions *result, Arr *assets, Arr *positions);

///
RsChampions *rankFlea_result (RankFlea *this);

/// Arr[RankAssets]
Arr *rankFlea_assets (RankFlea *this);

/// Arr[RankPosition]
Arr *rankFlea_positions (RankFlea *this);

///
Js *rankFlea_to_js (RankFlea *this);

///
RankFlea *rankFlea_from_js (Js *js);

/// Pair date-int to use with charts.
///   Arguments:
///     is_new: bool
///     variation: int
///     points: int
///     assets: int
///     model: char*
///     flea: char*
typedef struct Rank_Rank Rank;

///
Rank *rank_new (
  int is_new,
  int variation,
  int points,
  int assets,
  char *model,
  char *flea
);

///
int rank_is_new (Rank *this);

/// If is_new == 1, variation == 0. Otherwise variation can be in range [-2 - 2]
int rank_variation (Rank *this);

///
int rank_points (Rank *this);

///
int rank_assets (Rank *this);

///
char *rank_model (Rank *this);

///
char *rank_flea (Rank *this);

///
Js *rank_to_js (Rank *this);

///
Rank *rank_from_js (Js *js);

/*--*/

/// Returns Arr[Arr[RankPosition]] Calculate historic positions in ranking.
///   assets: (Arr[Arr[RankAssets]]) Table of assetes. Rows are fleas
///     values and fields are assets for each date
///   return: (Arr[Arr[RankPosition]]) Table of positions. Rows are fleas
///     values and fields are positions for each date
Arr *rank_mk_positions (Arr *assets);

/// Returns Arr[Rank]
///   rk     : Arr[RankAssetsEntry] Current ranking
///   prev_rk: Arr[RankAssetsEntry] Previous ranking
Arr *rank_mk_ranking (Arr *rk, Arr *prev_rk);

#endif
// Copyright 10-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Server to read quotes.

#ifndef DATA_SERVER_H
  #define DATA_SERVER_H

#include "dmc/async.h"

/*--*/

///
///   Arguments:
///     nick_id: int
///     code: Opt-char*
typedef struct Server_ServerCode ServerCode;

///
ServerCode *serverCode_new (int nick_id, Opt *code);

///
int serverCode_nick_id (ServerCode *this);

/// Opt[char]
Opt *serverCode_code (ServerCode *this);

///
Js *serverCode_to_js (ServerCode *this);

///
ServerCode *serverCode_from_js (Js *js);

///
///   Arguments:
///     id: int
///     short_name: char*
///   Variables:
///     name: char*
///     daily_conf: Opt-Rconf
///     historic_conf: Opt-Rconf
///     codes: Arr-ServerCode
typedef struct Server_Server Server;

///
int server_id (Server *this);

///
char *server_short_name (Server *this);

///
void server_set_short_name (Server *this, char *value);

///
char *server_name (Server *this);

///
void server_set_name (Server *this, char *value);

/// Opt[Rconf]
Opt *server_daily_conf (Server *this);

///
void server_set_daily_conf (Server *this, Opt *value);

/// Opt[Rconf]
Opt *server_historic_conf (Server *this);

///
void server_set_historic_conf (Server *this, Opt *value);

/// Arr[ServerCode]
Arr *server_codes (Server *this);

///
Js *server_to_js (Server *this);

///
Server *server_from_js (Js *js);

/*--*/

/// 'nicks' is Arr[Nick]
Server *server_new(int id, char *short_name, Arr *nicks);

/// If nick_id has not code, returns "".
char *server_nick_code (Server *this, int nick_id);

/// Code is "" if nick_id has not code.<p>
/// If this has not nick_id registred, this functions does nothing.
void server_set_nick_code (Server *this, int nick_id, char *code);

#endif
// Copyright 11-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Broker.

#ifndef DATA_FLEA_H
  #define DATA_FLEA_H

#include "dmc/async.h"
#include "Gen.h"

/*--*/

///
///   Arguments:
///     date: char*
///     cycle: int
///     id: int
///     gen: Gen
typedef struct Flea_Flea Flea;

///
char *flea_date (Flea *this);

///
int flea_cycle (Flea *this);

///
int flea_id (Flea *this);

///
Gen *flea_gen (Flea *this);

///
Js *flea_to_js (Flea *this);

///
Flea *flea_from_js (Js *js);

/*--*/

#endif

/// Creates a new flea.
///   date: Date of creation.
///   cycle: Cycle of creation.
///   id: Identifier number in cycle.
///   n: Number of elements of its gen.
Flea *flea_new (char *date, int cycle, int id, int n);

/// Returns a new Flea mutation of 'this', with a new identifier 'id'.
///   this: Flea
///   date: Date of creation.
///   cycle: Cycle of creation.
///   id: Identifier number in cycle.
Flea *flea_mutate (Flea *this, char *date, int cycle, int id);

/// Returns date-cycle-id
char *flea_name (Flea *this);

/// Calculates value of a gen parameter.
///   mx: Maximun value of parameter
///   mn: Minimum value of parameter
///   value: Gen parameter - between (0 and 1]
/// If mx <= mn returns 0.
double flea_param (double mx, double mn, double value);

// Copyright 04-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Server main.

#ifndef SERVER_H
  #define SERVER_H

#include "dmc/async.h"
#include "dmc/Iserver.h"

void server_run (AsyncActor *ac, Iserver *server);

#endif
// Copyright 23-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Entry point of acc pages requests.

#ifndef SERVER_ACC_H
  #define SERVER_ACC_H

#include "dmc/async.h"

/// mrq is Map[Js]
char *acc_process(AsyncActor *ac, Map *mrq);

#endif
// Copyright 04-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Requests entry point.

#ifndef SERVER_HUB_H
  #define SERVER_HUB_H

#include "dmc/async.h"

/// Process a request
char *hub_rp (AsyncActor *ac, char *rq);

#endif
// Copyright 26-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Requests from acc->balance

#ifndef SERVER_ACC_ACC__BALANCE_H
  #define SERVER_ACC_ACC__BALANCE_H

#include "dmc/async.h"

/// mrq is Map[Js]
char *acc__balance_process(AsyncActor *ac, Map *mrq);

#endif
// Copyright 25-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Requests from widget Downloader of acc.

#ifndef SERVER_ACC_ACC__DOWNLOADER_H
  #define SERVER_ACC_ACC__DOWNLOADER_H

#include "dmc/async.h"

/// mrq is Map[Js]
char *acc__downloader_process(AsyncActor *ac, Map *mrq);

#endif
// Copyright 27-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Requests from acc->profits

#ifndef SERVER_ACC_ACC__PROFITS_H
  #define SERVER_ACC_ACC__PROFITS_H

#include "dmc/async.h"

/// mrq is Map[Js]
char *acc__profits_process(AsyncActor *ac, Map *mrq);

#endif
// Copyright 24-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Requests from acc->companies

#ifndef SERVER_ACC_ACC__COMPANIES_H
  #define SERVER_ACC_ACC__COMPANIES_H

#include "dmc/async.h"

/// mrq is Map[Js]
char *acc__companies_process(AsyncActor *ac, Map *mrq);

#endif
// Copyright 27-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Requests from acc->trading

#ifndef SERVER_ACC_ACC__TRADING_H
  #define SERVER_ACC_ACC__TRADING_H

#include "dmc/async.h"

/// mrq is Map[Js]
char *acc__trading_process(AsyncActor *ac, Map *mrq);

#endif
// Copyright 28-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Entry point of daily requests.

#ifndef SERVER_DAILY_H
  #define SERVER_DAILY_H

#include "dmc/async.h"

/// mrq is Map[Js]
char *daily_process(AsyncActor *ac, Map *mrq);

#endif
// Copyright 21-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Requests from fleas->model

#ifndef SERVER_FLEAS_FLEAS__MODEL_H
  #define SERVER_FLEAS_FLEAS__MODEL_H

#include "dmc/async.h"

/// mrq is Map[Js]
char *fleas__model_process(AsyncActor *ac, Map *mrq);

#endif
// Copyright 19-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Requests from fleas->charts

#ifndef SERVER_FLEAS_FLEAS__CHARTS_H
  #define SERVER_FLEAS_FLEAS__CHARTS_H

#include "dmc/async.h"

/// mrq is Map[Js]
char *fleas__charts_process(AsyncActor *ac, Map *mrq);

#endif
// Copyright 21-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Requests from fleas->champions

#ifndef SERVER_FLEAS_FLEAS__CHAMPIONS_H
  #define SERVER_FLEAS_FLEAS__CHAMPIONS_H

#include "dmc/async.h"

/// mrq is Map[Js]
char *fleas__champions_process(AsyncActor *ac, Map *mrq);

#endif
// Copyright 30-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Requests from fleas->wgs->Wcharts

#ifndef SERVER_FLEAS_WGS_FLEAS__WGS__WCHARTS_H
  #define SERVER_FLEAS_WGS_FLEAS__WGS__WCHARTS_H

#include "dmc/async.h"

/// mrq is Map[Js]
char *fleas__wgs__wcharts_process(AsyncActor *ac, Map *mrq);

#endif
// Copyright 30-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Requests from fleas->wgs->Dtable

#ifndef SERVER_FLEAS_WGS_FLEAS__WGS__DTABLE_H
  #define SERVER_FLEAS_WGS_FLEAS__WGS__DTABLE_H

#include "dmc/async.h"

/// mrq is Map[Js]
char *fleas__wgs__dtable_process(AsyncActor *ac, Map *mrq);

#endif
// Copyright 19-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Requests from fleas->bests

#ifndef SERVER_FLEAS_FLEAS__BESTS_H
  #define SERVER_FLEAS_FLEAS__BESTS_H

#include "dmc/async.h"

/// mrq is Map[Js]
char *fleas__bests_process(AsyncActor *ac, Map *mrq);

#endif
// Copyright 19-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Requests from fleas->main

#ifndef SERVER_FLEAS_FLEAS__MAIN_H
  #define SERVER_FLEAS_FLEAS__MAIN_H

#include "dmc/async.h"

/// mrq is Map[Js]
char *fleas__main_process(AsyncActor *ac, Map *mrq);

#endif
// Copyright 08-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Requests from sys->schedule

#ifndef SERVER_SYS_SYS__SCHEDULE_H
  #define SERVER_SYS_SYS__SCHEDULE_H

#include "dmc/async.h"

/// mrq is Map[Js]
char *sys__schedule_process(AsyncActor *ac, Map *mrq);

#endif
// Copyright 01-Jul-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Requests from sys->annotations

#ifndef SERVER_SYS_SYS__ANNOTATIONS_H
  #define SERVER_SYS_SYS__ANNOTATIONS_H

#include "dmc/async.h"

/// mrq is Map[Js]
char *sys__annotations_process(AsyncActor *ac, Map *mrq);


#endif
// Copyright 05-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Requests from sys->main

#ifndef SERVER_SYS_SYS__MAIN_H
  #define SERVER_SYS_SYS__MAIN_H

#include "dmc/async.h"

/// mrq is Map[Js]
char *sys__main_process(AsyncActor *ac, Map *mrq);

#endif
// Copyright 06-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Requests from sys->nicks

#ifndef SERVER_SYS_SYS__NICKS_H
  #define SERVER_SYS_SYS__NICKS_H

#include "dmc/async.h"

/// mrq is Map[Js]
char *sys__nicks_process(AsyncActor *ac, Map *mrq);

#endif
// Copyright 11-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Requests from sys->servers

#ifndef SERVER_SYS_SYS__SERVERS_H
  #define SERVER_SYS_SYS__SERVERS_H

#include "dmc/async.h"

/// mrq is Map[Js]
char *sys__servers_process(AsyncActor *ac, Map *mrq);

#endif
// Copyright 12-Jul-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Requests from sys->models

#ifndef SERVER_SYS_SYS__MODELS_H
  #define SERVER_SYS_SYS__MODELS_H

#include "dmc/async.h"

/// mrq is Map[Js]
char *sys__models_process(AsyncActor *ac, Map *mrq);

#endif
// Copyright 05-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Requests from sys->chpass

#ifndef SERVER_SYS_SYS__CHPASS_H
  #define SERVER_SYS_SYS__CHPASS_H

#include "dmc/async.h"

/// mrq is Map[Js]
char *sys__chpass_process(AsyncActor *ac, Map *mrq);

#endif
// Copyright 05-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Requests from sys->settings

#ifndef SERVER_SYS_SYS__SETTINGS_H
  #define SERVER_SYS_SYS__SETTINGS_H

#include "dmc/async.h"

/// mrq is Map[Js]
char *sys__settings_process(AsyncActor *ac, Map *mrq);

#endif
// Copyright 05-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Requests from sys->backups

#ifndef SERVER_SYS_SYS__BACKUPS_H
  #define SERVER_SYS_SYS__BACKUPS_H

#include "dmc/async.h"

/// mrq is Map[Js]
char *sys__backups_process(AsyncActor *ac, Map *mrq);

#endif
// Copyright 22-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Requests from sys->servers->download

#ifndef SERVER_SYS_SERVERS_SYS__SERVERS__DOWNLOAD_H
  #define SERVER_SYS_SERVERS_SYS__SERVERS__DOWNLOAD_H

#include "dmc/async.h"

/// mrq is Map[Js]
char *sys__servers__download_process(AsyncActor *ac, Map *mrq);

#endif
// Copyright 20-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Requests from sys->servers->names

#ifndef SERVER_SYS_SERVERS_SYS__SERVERS__NAMES_H
  #define SERVER_SYS_SERVERS_SYS__SERVERS__NAMES_H

#include "dmc/async.h"

/// mrq is Map[Js]
char *sys__servers__names_process(AsyncActor *ac, Map *mrq);

#endif
// Copyright 21-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Requests from sys->servers->codes

#ifndef SERVER_SYS_SERVERS_SYS__SERVERS__CODES_H
  #define SERVER_SYS_SERVERS_SYS__SERVERS__CODES_H

#include "dmc/async.h"

/// mrq is Map[Js]
char *sys__servers__codes_process(AsyncActor *ac, Map *mrq);

#endif
// Copyright 05-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Requests from sys->home

#ifndef SERVER_SYS_SYS__HOME_H
  #define SERVER_SYS_SYS__HOME_H

#include "dmc/async.h"

/// mrq is Map[Js]
char *sys__home_process(AsyncActor *ac, Map *mrq);

#endif
// Copyright 16-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Requests from sys->nicks->wnick

#ifndef SERVER_SYS_NICKS_SYS__NICKS__WNICK_H
  #define SERVER_SYS_NICKS_SYS__NICKS__WNICK_H

#include "dmc/async.h"

/// mrq is Map[Js]
char *sys__nicks__wnick_process(AsyncActor *ac, Map *mrq);

#endif
// Copyright 31-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Requests from sys->nicks->editor

#ifndef SERVER_SYS_NICKS_SYS__NICKS__EDITOR_H
  #define SERVER_SYS_NICKS_SYS__NICKS__EDITOR_H

#include "dmc/async.h"

/// mrq is Map[Js]
char *sys__nicks__editor_process(AsyncActor *ac, Map *mrq);

#endif
// Copyright 19-Jun-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Entry point of fleas pages requests.

#ifndef SERVER_FLEAS_H
  #define SERVER_FLEAS_H

#include "dmc/async.h"

/// mrq is Map[Js]
char *fleas_process(AsyncActor *ac, Map *mrq);

#endif
// Copyright 04-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Entry point of sys pages requests.

#ifndef SERVER_SYS_H
  #define SERVER_SYS_H

#include "dmc/async.h"

/// mrq is Map[Js]
char *sys_process(AsyncActor *ac, Map *mrq);

#endif
// Copyright 07-Aug-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Entry point of ranking requests.

#ifndef SERVER_RANKING_H
  #define SERVER_RANKING_H

#include "dmc/async.h"

/// mrq is Map[Js]
char *ranking_process(AsyncActor *ac, Map *mrq);

#endif
// Copyright 03-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Global definitions.

#ifndef DEFS_H
  #define DEFS_H

#include "dmc/async.h"

///
#define APP_NAME "MultiMarket"

/// Log maximum entries
#define LOG_MAX_ENTRIES 300

/// Communications port
#define PORT 50286

/// Time to server sleep (milliseconds)
#define ACTOR_SLEEP 10

/// Time to server sleep (milliseconds)
#define SERVER_SLEEP 10

/// Time to server sleep (milliseconds) when reading errors
#define SERVER_ERROR_SLEEP 120000

/// Time to scheduler sleep (milliseconds)
#define SCHEDULER_SLEEP 50

/// Time of connection expiration
#define EXPIRATION 900

/// Data version
#define DATA_VERSION "MultiMarket\nData version: 201905\n"

/// Automatic backups number
#define BACKUPS_NUMBER 10

/// Number of quotes in historic
#define HISTORIC_QUOTES 610

/// Minimal number of companies in a set
#define SET_COMPANIES 10

/// Number of quotes to calculate volume
#define VOLUME_QUOTES 100

/// Activity state
#define ACT_SLEEPING1 "Sleeping (1)"

/// Activity state
#define ACT_HISTORIC "Historic"

/// Activity state
#define ACT_SLEEPING2 "Sleeping (2)"

/// Activity state
#define ACT_ACTIVATING "Activating"

/// Activity state
#define ACT_ACTIVE "Active"

/// Activity state
#define ACT_DEACTIVATING "Deactivating"

/// Hour to start ACT_HISTORIC fase
#define ACT_HISTORIC_START 3

/// Hour to finish ACT_HISTORIC fase
#define ACT_HISTORIC_END 8

/// Servers delay in SECONDS
#define SERVERS_DELAY 1200

/// Maximum of mutation
#define MUTATION_MULTIPLIER 0.3

/// Number of cycle after insertion to finish a process for parameter
#define CYCLES 5

/// Number of cycle to insert historic results
#define INSERTION_CYCLE 10

/// Number of fleas per model
#define FLEAS_PER_MODEL 2000

/// Fleas initial capital for each cycle
#define INITIAL_CAPITAL 150000

/// Bet
#define BET 15000

/// Minimun cash to bet
#define MIN_TO_BET 16000

/// Minimum operations to survive (divisor: days / minSells)
#define MIN_SELLS 30

/// Maximun operations to survive (divisor: days / maxSells)
#define MAX_SELLS 15

/// Maximun number of fleas in "_best"
#define MAXIMUM_HISTORIC_BESTS 252

/// Number of daily results in data base 'data/fleas/MODEL'
#define FLEA_MODEL_DATES 10

/// Avg days of champions
#define CHAMPIONS_AVG 10

/// Number total of champion fleas per group
#define TOTAL_CHAMPIONS 500

/// Quotes number in account charts
#define ACC_CHART_QUOTES 250

/// Ponderation for raking
#define RANKING_ASSETS_RATIO 0.5

/// Ponderation for raking
#define RANKING_PROFITS_RATIO 0.3

/// Ponderation for raking
#define RANKING_AGE_RATIO 0.2

/// Number of fleas in pool
#define RANKING_POOL_NUMBER 1000

/// Number of fleas in ranking
#define RANKING_NUMBER 40

/// Number of fleas in ranking
#define HISTORIC_RANKING_ENTRIES 10

/// Maximum number of data in ranking charts
#define HISTORIC_RANKING_CHAR_MAX 450

/// Server short name to get url in accounting charts
#define ACC_URL "INFOB"

/// Error messages
enum ErrorMsg { MSG_OK, MSG_WARNING, MSG_ERROR };

/// Server configuration states
enum Server { SERVER_STOPPED, SERVER_ACTIVE, SERVER_SELECTED };

#endif
// Copyright 29-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Accounting.

#ifndef SCHEDULER_ACC_H
  #define SCHEDULER_ACC_H

#include "dmc/async.h"

/// Adds profits to historic (acc/profits data base).
void acc_historic_profits (AsyncActor *ac);

#endif
// Copyright 29-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Historic data management.

#ifndef SCHEDULER_HISTORIC_H
  #define SCHEDULER_HISTORIC_H

#include "dmc/async.h"

/// Updates historic data
void historic_update (AsyncActor *ac);

#endif
// Copyright 29-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Fleas main.

#ifndef SCHEDULER_FLEAS_H
  #define SCHEDULER_FLEAS_H

#include "dmc/async.h"

/// Running fleas in thread apart
void fleas_run(AsyncActor *ac);

#endif
// Copyright 12-Jul-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Update matches between companies and flea models

#ifndef SCHEDULER_MANAGEMENT_H
  #define SCHEDULER_MANAGEMENT_H

#include "dmc/async.h"

/// Update matches between companies and flea models
void management_update (AsyncActor *ac);

#endif
// Copyright 04-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Scheduler main.

#ifndef SCHEDULER_H
  #define SCHEDULER_H

#include "dmc/async.h"

///
void scheduler_run (AsyncActor *null);

#endif
// Copyright 03-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Application main.

#ifndef MULTIMARKET_H
  #define MULTIMARKET_H

#include "dmc/async.h"

///
int main (int argc, char *argv[]);

#endif
// Copyright 15-Oct-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Array structure.

#ifndef DMC_ARR_H
  #define DMC_ARR_H

#include "DEFS.h"
#include "Tp.h"
#include "Tp3.h"

typedef struct js_Js Js;

typedef struct it_It It;

///
typedef struct arr_Arr Arr;

/// Creates a new Array with buffer size of 15 elements.
Arr *arr_new (void);

/// 'buffer' must be > 0.
Arr *arr_bf_new (int buffer);

/// Creates a new array from several elements.
Arr *arr_new_from (void *e, ...);

/// Creates a new array from a C array. For example:
///   Arr *a = arr_new_c(3, (void *[]){"c", "d", "e"});
/// If 'size' is less than C array length, result is ok (only will be
/// used 'size' first elements); but if 'size' is greater, the result is
/// undetermined.
Arr *arr_new_c (int size, void **es);

/// Returns a new array with elements of 'this'.
Arr *arr_copy (Arr *this);

///
int arr_size (Arr *this);

/// Resturn the element at position ix.
void *arr_get (Arr *this, int ix);

/// Return a pointer to the first element of 'this'
void **arr_start (Arr *this);

/// Returns a pointer to the next element after the last element of 'this'.
/// 'arr_end' does not point to a valid element.
void **arr_end (Arr *this);

/// Adds an element at the end of 'this'. 'e' will be freed by 'this'.
void arr_push (Arr *this, void *e);

/// Returns and removes the last element.
void *arr_pop (Arr *this);

/// Returns the las element.
void *arr_peek (Arr *this);

/// Sets the element at position ix.
void arr_set (Arr *this, int ix, void *e);

/// Inserts an element at position ix.
void arr_insert (Arr *this, int ix, void *e);

/// Removes an element at position ix. Buffer size of 'this' does not change.
void arr_remove (Arr *this, int ix);

/// Adds pointer to elements of 'other' to 'this'.
void arr_cat (Arr *this, Arr *other);

/// Inserts pointer to elements of 'other' at 'ix'
void arr_insert_arr (Arr *this, int ix, Arr *other);

/// Removes elements between [begin-end). Buffer size of 'this' does not change.
void arr_remove_range (Arr *this, int begin, int end);

/// Removes every element of 'this'. Buffer size is equals to 15.
void arr_clear (Arr *this);

/// Removes every element of 'this'.
void arr_bf_clear (Arr *this, int buffer);

/// Reverses elements of 'this'.
void arr_reverse (Arr *this);

/// Sorts 'this' ascendantly using the function 'greater' that returns 'true'
/// if 'e1' > 'e2'.
void arr_sort (Arr *this, int (*greater)(void *e1, void *e2));

/// arr_shuflle remix 'this' elements. It should be used after calling
/// rnd_init() or sys_init().
void arr_shuffle (Arr *this);

/// Returns '1' if every element of 'this' yields '1' with 'pred'.
int arr_all (Arr *this, int (*pred)(void *e));

/// Returns '1' if some element of 'this' yields '1' with 'pred'.
int arr_any (Arr *this, int (*pred)(void *e));

/// Returns the index of the first elements which returns 'true'
/// with 'pred', or -1 if such element does not exist.
int arr_index (Arr *this, int (*pred)(void *e));

/// Returns the index of the last elements which returns 'true'
/// with 'pred', or -1 if such element does not exist.
int arr_last_index (Arr *this, int (*pred)(void *e));

/// arr_filter_in removes every element which returns 'false' with 'pred'.
void arr_filter_in (Arr *this, int (*pred)(void *e));

/// Returns a new Arr. See it_take.
Arr *arr_take (Arr *this, int n);

/// Returns a new Arr. See it_takef.
Arr *arr_takef (Arr *this, int (*predicate)(void *e));

/// Returns a new Arr. See it_drop.
Arr *arr_drop (Arr *this, int n);

/// Returns a new Arr. See it_dropf.
Arr *arr_dropf (Arr *this, int (*predicate)(void *e));

/// Returns a new Arr. See it_filter.
Arr *arr_filter_to (Arr *this, int (*predicate)(void *e));

/// Returns a new Arr. See it_map.
Arr *arr_map (Arr *this, void *(*converter)(void *e));

/// Returns a new Arr. See it_map2.
Arr *arr_map2 (Arr *this, void *(*conv1)(void *e), void *(*conv2)(void *e));

/// Returns a new Arr. Returns Arr[Tp]. See it_zip.
Arr *arr_zip (Arr *a1, Arr *a2);

/// Returns a new Arr. Returns Arr[Tp3]. See it_zip3.
Arr *arr_zip3 (Arr *a1, Arr *a2, Arr *a3);

/// Returns Tp[Arr, Arr] from an Arr[Tp]. 'Return_e1' contains elements of
/// source 'Tp_e1' and 'return_e2' elementso of 'Tp_e2'.
Tp *arr_unzip (Arr *this);

/// Returns Tp[Arr, Arr, Arr] from an Arr[Tp2]. 'Return_e1' contains elements
/// of source 'Tp_e1', 'return_e2' elements of 'Tp_e2' and 'return_e3'
/// elements of 'Tp_e3'.
Tp3 *arr_unzip3 (Arr *this);

/// Returns Tp[Arr, Arr] (duplicates, rest) See it_duplicates.
Tp *arr_duplicates (Arr *this, int (feq)(void *e1, void *e2));

/// Creates an iterator over 'this'.
It *arr_to_it (Arr *this);

/// Creates an Arr from 'it'.
Arr *arr_from_it (It *it);

/// Returns a Js from an element of 'this'
Js *arr_to_js (Arr *this, Js *(*to)(void *e));

/// Parses a Js to an element of 'this'.
Arr *arr_from_js (Js *js, void *(*from)(Js *jse));

#endif
// Copyright 29-May-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Utilities to manage exceptions.
/// Only functions 'exc_init', 'exc_stack'  and 'exc_throw' must be used
/// directly. The rest must be used through the macros TRY-CATCH-_TRY and
/// THROW-_THROW. (See "?libdmc@dmc/DEFS#hp:TRY").

#ifndef DM_EXC_H
  #define DM_EXC_H

#include <setjmp.h>

typedef struct arr_Arr Arr;

///
typedef struct exc_Exc Exc;

/// Returns exception type. Predefined types are:
///   exc_generic_t
///   exc_range_t
///   exc_illegal_argument_t
///   exc_illegal_state_t
///   exc_io_t
char *exc_type (Exc *this);

/// Returns exception message.
char *exc_msg (Exc *this);

/// Returns Arr[char]. The exception stack trace.
Arr *exc_stack (Exc *this);

/// Intializes jumps buffer. This function has to be called before using macros
/// TRY-CATCH-FINALLY-_TRY or THROW-_THROW.
void exc_init ();

/// Initializes thread data. It is intended to beeng use only
/// by 'async_thread'.
void exc_thread_init (void);

/// Removes a thread data. It is intended to beeng use only by 'async_thread'.
void exc_thread_end (void);

/// Adds a exception to buffer of current Exc in current thread.
void exc_add (jmp_buf *jump);

/// Returns current Exc in current thread.
Exc *exc_get(void);

/// Removes the top of jumps buffer of current Exc in current thread.
void exc_remove ();

/// Sends an exception.
/// If no TRY block has been defined it stops the program.
///   type   : Excepion type.
///   message: Message to show.
///   file   : Error file.
///   func   : Error function.
///   line   : Error line number.
void exc_throw (
  char *type, char *message, char *file, char *func, int line
);

///
#define exc_generic_t "generic"

///
#define exc_range_t "range"

/// Exception for index out of range.
///   begin: Lower limit inclusive.
///   end  : Upper limit inclusive.
///   index: The index out of range (< begin and > end).
char *exc_range (int begin, int end, int index);

///
#define exc_illegal_argument_t "argument"

/// Exception for argument with a wrong value.
///   argument_name: Name of wrong argument.
///   expected     : Value expected.
///   actual       : Actual value.
char *exc_illegal_argument (char *argument_name, char *expected, char *actual);

///
#define exc_illegal_state_t "state"

/// Exception for attempting to use an object in wrong state.
///   cause: Description of problem.
char *exc_illegal_state (char *cause);

///
#define exc_io_t "io"

/// Exception for Input - Output error.
///   cause: Description of problem.
char *exc_io (char *cause);

#endif
// Copyright 16-Oct-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Structure for working with bytes. Example:
///   Bytes *bs = bytes_new();
///   const unsigned char bss[] = {0, 23, 116, 225};
///   bytes_add_bytes(bs, bss, 4);
///   char b41 = b41_encodeBytes(bs);
///   assert(!strcmp("RRoixx", b41));
///   bytes_free(bs);

#ifndef DMC_BYTES_H
  #define DMC_BYTES_H

#include "Js.h"

///
typedef struct bytes_Bytes Bytes;

///
Bytes *bytes_new(void);

///
Bytes *bytes_bf_new(int length);

/// Returns a new allocated 'Bytes' whitch is copy of 'bs'.
Bytes *bytes_from_bytes(unsigned char *bs, int length);

/// Returns a 'Bytes' whitch is copy of 's' without the ending zero.
Bytes *bytes_from_str(char *s);

///
unsigned char *bytes_bs(Bytes *this);

///
int bytes_len(Bytes *this);

/// Adds to 'this' a new copy of 'bs'.
void bytes_add_bytes(Bytes *this, unsigned char *bs, int length);

/// Adds to 'this' a new copy of 'another'.
void bytes_add(Bytes *this, Bytes *another);

/// Adds to 'this' a copy of 's' without the ending zero.
void bytes_add_str(Bytes *this, char *s);

///
Js *bytes_to_js(Bytes *this);

///
Bytes *bytes_from_js(Js *js);

#endif
// Copyright 13-Dec-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#ifndef DMC_REGEX_H
  #define DMC_REGEX_H

/// Regular expressions management.

#include "dmc/std.h"

/// Returns the offets where ereg is found in s.
///   rex   : Regular expression to find. It does not admit grouping (that is,
///           parenthesis).
///   s     : String to search in.
///   return: Opt[Arr[Itp]]. Offsets of 's' with elements which match. Each
///           'Itp' contains [ofsset start inclusive - ofsect - end exclusive].
///           If any error happens, it returns opt_empty.
Opt *regex_matches (char *rex, char *s);

/// Equals to 'regex_matches' but "ignoring case".
Opt *regex_matches_ic (char *rex, char *s);

/// Replace ocurrences of 'rex' in 's' by 'replacement'. Ocurrences are
/// find by regex_matches.
///   rex        : Regular expression to replace. It does not admit grouping (that
///                is, parenthesis).
///   s          : String to search in.
///   replacement: New string.
///   return     : Opt[char]. A new string with repacements done.
///                If any error happens, it returns opt_empty.
Opt *regex_replace (char *rex, char *s, char *replacement);

/// Equals to regex_replace, but "ignoring case".
Opt *regex_replace_ic (char *rex, char *s, char *replacement);

#endif
// Copyright 15-Oct-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Array of doubles.

#ifndef DMC_DARR_H
  #define DMC_DARR_H

#include "Js.h"

///
typedef struct darr_Darr Darr;

///
Darr *darr_new(void);

/// buffer must be > 0.
Darr *darr_bf_new(int buffer);

/// Creates a new array from a C array. For example:
///   Darr *a = darr_new_c(3, (double[]){1.0, 1.8, 1.3});
/// If 'size' is less than C array length, result is ok (only will be
/// used 'size' first elements); but if 'size' is greater, the result is
/// undetermined.
Darr *darr_new_c (int size, double *es);

/// Returns a new Darr with elements from 0 to 'ix' (exclusive),
Darr *darr_left(Darr *this, int ix);

/// Returns a new Darr with elements from 'ix' (inclusive) to end of 'this'.
Darr *darr_right(Darr *this, int ix);

/// Returns a new Darr with elements from 'begin' (inclusive) to
/// to 'end' (exclusive),
Darr *darr_sub(Darr *this, int begin, int end);

///
Darr *darr_copy(Darr *this);

///
int darr_eq(Darr *this, Darr *other, double gap);

///
int darr_size(Darr *this);

/// If ix is < 0 then is changed to 'darr_size - ix'.
double darr_get(Darr *this, int ix);

///
double *darr_start(Darr *this);

///
double *darr_end(Darr *this);

///
void darr_push(Darr *this, double e);

/// If ix is < 0 then is changed to 'darr_size - ix'.
void darr_set(Darr *this, int ix, double e);

/// If ix is < 0 then is changed to 'darr_size - ix'.
void darr_insert(Darr *this, int ix, double e);

/// If ix is < 0 then is changed to 'darr_size - ix'.
void darr_remove(Darr *this, int ix);

///
void darr_cat(Darr *this, Darr *other);

/// If ix is < 0 then is changed to 'darr_size - ix'.
void darr_insert_arr(Darr *this, int ix, Darr *other);

/// If begin or end are < 0 then is changed to 'darr_size - itsValue'.
void darr_remove_range(Darr *this, int begin, int end);

/// Removes every element of 'this'. Buffer size is equals to 15.
void darr_clear (Darr *this);

/// Removes every element of 'this'.
void darr_bf_clear (Darr *this, int buffer);

///
void darr_reverse(Darr *this);

///
void darr_sort(Darr *this);

///
Js *darr_to_js(Darr *this);

///
Darr *darr_from_js(Js *js);

#endif
// Copyright 13-Dec-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

#ifndef DMC_ITP_H
  #define DMC_ITP_H

/// Tuple of two integers.

#include "dmc/std.h"

///
typedef struct itp_Itp Itp;

///
Itp *itp_new(int i1, int i2);

///
int itp_e1(Itp *this);

///
int itp_e2(Itp *this);

#endif
// Copyright 23-Apr-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Hash (immutable) structure.

#ifndef DM_HASH_H
  #define DM_HASH_H

#include "dmc/Arr.h"
#include "dmc/Opt.h"

///
typedef struct hash_Hash Hash;

/// Initializates a Hash. Hash can be cast to Map<Kv>.
Hash *hash_new(void);

/// Returns the number of elements. (O(n) operation).
int hash_count(Hash *this);

/// Puts 'value' with key 'key'. If key already exists, it will not
/// be deleted; but will not be used.
///   this : The Hash.
///   key  : Entry key.
///   value: New value
Hash *hash_put(Hash *this, char *key, void *value);

/// Returns 1 if 'this' has key 'key'.
int hash_has_key(Hash *this, char *key);

/// Returns the value pointed by key or 'opt_empty' if 'key' does.
/// not exist.
Opt *hash_get(Hash *this, char *key);

/// Returns a new hash with duplicates removed.
Hash *hash_compact(Hash *this);

/// Returns a new hash with the entry with key 'key' removed.
Hash *hash_remove(Hash *this, char *key);

/// Returns keys of this in a Arr[char] with duplicated keys removed.
Arr *hash_keys(Hash *this);

/// Returns an Arr<Kv> with dulicated keys removed.
Arr *hash_kvs(Hash *this);

/// Creates an iterator over 'this'. Duplicates are removed.
///   return: It<Kv>
It *hash_to_it(Hash *this);

/// Creates a Hash from a 'it'.
///   it: It<Kv>
Hash *hash_from_it(It *it);

/// Returns a Js from a value of 'this'. Duplicates are removed.
///   to: Value converter.
Js *hash_to_js(Hash *this, Js *(*to)(void *e));

/// Parses a Js to a value of 'this'.
///   from: Value converter.
Hash *hash_from_js(Js *js, void *(*from)(Js *jse));

#endif
// Copyright 23-Apr-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Pair key - value.

#ifndef DMC_KV_H
  #define DMC_KV_H

///
typedef struct kv_Kv Kv;

///
Kv *kv_new(char *key, void *value);

///
char *kv_key(Kv *this);

///
void *kv_value(Kv *this);

#endif
// Copyright 17-Oct-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Generator of random numbers.

#ifndef DMC_RND_H
  #define DMC_RND_H

#include "dmc/Arr.h"

///
typedef struct rnd_Box Box;

/// Intializates the random number generator.
void rnd_init(void);

/// Generates a new double between 0.0 (inclusive) and 1.0 (exclusive).
double rnd_d(void);

/// Generates a new int between 0 (inclusive) and 'top' (exclusive).
int rnd_i(int top);

/// Returns an 'Box' that iterates over 'a' elements randomly. When it finishes
/// with all the elements of 'a', restarts again.
Box *rnd_box_new(Arr *a);

/// Returns the next element of 'this'.
void *rnd_box_next(Box *this);

#endif
// Copyright 29-Apr-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Management of multithread programs.
/// NOTE: This file must be included instead of 'std.h'.

#ifndef DMC_ASYNC_H
  #define DMC_ASYNC_H

#include <pthread.h>
/// Necesary definition for multithreading garbage collector.
#define GC_THREADS
#include "gc.h"
#include "dmc/std.h"

/// Launchs 'fn' in a new joinable thread. Example of use:
///   void fn() { puts("Here"); }
///   pthread_t *thr = async_thread(fn);
///   async_join(thr); // Waits for thr.
/// NOTE: After calling 'async_thread' is mandatory to call 'async_join' to
///         free resources.
pthread_t *async_thread (void (*fn)(void));

/// Launchs 'fn' in a new joinable thread. Example of use:
///   void fn(char *tx) { puts(tx); }
///   pthread_t *thr = async_thread2((FPROC)fn, "Hello");
///   async_join(thr); // Waits for thr.
/// NOTE: After calling 'async_thread' is mandatory to call 'async_join' to
///         free resources.
pthread_t *async_thread2 (void (*fn)(void *), void *value);

/// Launch 'fn' in a new thread. Example of use:
///   void fn(char *tx) { puts(tx); }
///   async_thread_detached((FPROC)fn, "Hello");
void async_thread_detached (void (*fn)(void *), void *value);

/// Wait until thr finishes.
void async_join (pthread_t *thr);

///
typedef struct async_AsyncActor AsyncActor;

/// 'millis' is the latence time.
AsyncActor *asyncActor_new (int millis);

/// Executes 'fn(value)' synchronicaly. This function returns immediatly.
void asyncActor_run (AsyncActor *this, void (*fn)(void *), void *value);

/// Executes 'fn(value)' synchronicaly. This function stops the program
/// until 'fn' is finished.
void asyncActor_wait (AsyncActor *this, void (*fn)(void));

/// Finalizes 'this'. 'this' also will finish is pendant jobs.
void asyncActor_end (AsyncActor *this);

/// Waits until 'this' is finished.
void asyncActor_join (AsyncActor *this);

///
typedef struct async_AsyncTimer AsyncTimer;

/// Executes 'fn(value)' each 'millis' milliseconds.
AsyncTimer *asyncTimer_new (void (*fn)(void *), void *value, int millis);

/// Finalizes 'this'.
void asyncTimer_end (AsyncTimer *this);

#endif
// Copyright 17-Oct-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Cryptographic utilities.

#ifndef DMC_CRYP_H
  #define DMC_CRYP_H

/// Generates a b64 random key of a length 'lg'.
char *cryp_genk (int lg);

/// Codified 'k' in irreversible way, using 'lg' b64 digits..
///   k : String to codify.
///   lg: Length of result.
char *cryp_key (char *k, int lg);

/// Encodes 's' with key 'k'.
///   s: Message to encode.
///   k: Key for encoding.
char *cryp_cryp (char *s, char *k);

/// Decodes 'b64' using key 'k'. 'b64' was codified with cryp().
///   b64: Text codified with cryp().
///   k  : Key for decoding.
char *cryp_decryp (char *b64, char *k);

/// Encodes automatically 's' with a random key of 'nk' digits.
///   nK: Number of digits for random key (1 to 64 both inclusive).
///   s : Text for encoding.
char *cryp_auto_cryp (char *s, int nk);

/// Decodes a text codified with autoCryp().
///   b64: Codified text.
char *cryp_auto_decryp (char *b64);

/// Encodes 's' whith key 'k' and an autoKey of length 'nK'.
///   k : Key for encoding.
///   nk: Digits to generate autoKey (1 to 40 both inclusive).
///   s : Message to encode.
char *cryp_encode (char *s, int nk, char *k);

/// Decodes a string codified with encode()
///   b64: Message encoded with encode().
///   k  : Key for encoding.
char *cryp_decode (char *b64, char *k);

#endif
// Copyright 18-Oct-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Utilities for HTML conections between client - server.

#ifndef DMC_CGI_H
  #define DMC_CGI_H

#include "dmc/std.h"

/// Returns of cgi_get_session_data.
typedef struct cgi_Session CgiSession;

/// Returns comunication key.
char *cgiSession_key(CgiSession *this);

/// Returns connection identifier.
char *cgiSession_id(CgiSession *this);

/// Returns the standard length of keys.
int cgi_klen(void);

/// Initializes a new interface of commnications.
///   home        : Aboslute path of application directory. For example:
///                   "/peter/wwwcgi/dmcgi/JsMon"
///                   or
///                   "/home/deme/.dmCApp/JsMon" ).
///   t_expiration: Time in seconds.
void cgi_init(char *home, time_t t_expiration);

/// Returns 'home' directory.
char *cgi_home(void);

/// Sets the key which 'cgi_ok' and 'cgi_err' will use. This function is called
/// when connection or authentication.
void cgi_set_key(char *k);

/// Returns Opt<CgiSession>.
/// If 'session_id' is not valid, it returns 'opt_empty'.
Opt *cgi_get_session(char *session_id);

/// Adds an user to database.
///   admin : Admin name.
///   akey  : Admin password.
///   user  : New user name.
///   ukey  : New user password.
///   level : New user level. Level "0" is the admin one.
///   return: A boolean field {ok:true|false}, sets to true if operations
///           succeeded. A fail can come up if 'admin' authentication fails.
char *cgi_add_user(
  char *admin, char *akey,
  char *user, char *ukey, char *level
);

/// Removea an user from database.
///   admin : Admin name.
///   akey  : Admin password.
///   user  : User name to remove.
///   return: A boolean field {ok:true|false}, sets to true if
///           operation succeeded. A fail can come up if 'admin' authentication
///           fails.
char *cgi_del_user(char *admin, char *akey, char *user);

/// Modifies user level.
///   admin : Admin name.
///   akey  : Admin password.
///   user  : User name to change level.
///   level : New user level. Level "0" is the admin one.
///   return: A boolean field {ok:true|false}, sets to true if
///           operation succeeded. A fail can come up if 'admin' authentication
///           fails or 'user' does not exist.
char *cgi_change_level(
  char *admin, char *akey, char *user, char *level
);

/// Changes user password.
///   user   : User name to change password.
///   key    : Old password.
///   new_key: New password.
///   return : A boolean field {ok:true|false}, sets to true if operation
///            succeeded. A fail can come up if 'user' authentication fails.
char *cgi_change_pass(char *user, char *key, char *new_key);

/// cgi_del_session deletes 'session' and returns an empty response.
char *cgi_del_session(char *session_id);

/// Sends to client level, key, page_id and session_id.
/// If authentication failed every value is "".
///   user      : User name.
///   key       : User password.
///   expiration: If is set to false, session will expire after 30 days.
///   return    : 'level', 'key', 'pageId' and 'sessionId' fields.
char *cgi_authentication(char *user, char *key, int expiration);

/// Returns client 'connectionId' and 'key'. If conection failed both are "".
///   session_id: Session identifier.
///   return    : {connectionId: String, key: String}.
///               'key' is a new key, set for the new connection.
char *cgi_connect(char  *session_id);

/// Returns a normal response.
/// 'data' is a Map[Js]
char *cgi_ok(Map *data);

/// Retuns an empty response.
char *cgi_empty(void);

/// Returns an error response, setting {error:msg}.
char *cgi_error(char *msg);

/// Returns a expired response, setting {expired:1}.
char *cgi_expired(void);

/// Runs a "long run" task. This function is intended to be called until it
/// returns {"longRunEnd"='true'}.
///   fn    : Map[Js] *(*)(void *ctx, Map[Js *rq]). "Long run" task. <i>It must
///           not be defined as inner function</i>.
///   ctx   : Context. It can be NULL. Value to pass to fn.
///   rq    : Map[js]. Data for 'fn'. 'rq' must have a field called
///           "longRunFile" which value the first time it is called is "" and
///           after
///           that its value is the returned by this function.
///   return:
///     first call     : A Map[Js] with an only field "longRunFile" whitch must
///                      be used in following calls.
///     following calls: - If 'fn' was finished the Map returned with 'fn' with
///                        the field {"longRunEnd"='true'} added.
///                      - If 'fn' is running a Map with the only field
///                        {"longRunEnd"='false'}
Map *cgi_long_run(Map *(*fn)(void *ctx, Map *rq), void *ctx, Map *rq);
#endif
// Copyright 30-May-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Iterator.

#ifndef DM_IT_H
  #define DM_IT_H

#include <stddef.h>
#include "Arr.h"

typedef struct opt_Opt Opt;

///
typedef struct it_It It;

///
typedef Opt *(*it_Next)(void *);

/// it_new creates a new It.
///   o   : Container
///   next: Function which returns a element of 'o' or Opt_empty() if there
///         are no more elements.
It *it_new (void *o, Opt *(*next)(void *o));

///
It *it_empty (void);

///
It *it_unary (void *e);

/// it_range is an iterator that returns values between begin (inclusive)
/// and end (exclusive).
It *it_range (int begin, int end);

/// it_range0 is equals to it_range(0, end).
It *it_range0 (int end);

///
int it_has_next (It *this);

///
void *it_next (It *this);

/// Shows next element witout advancing.
void *it_peek (It *this);

/// it_add adds an element at the end of 'this's
It *it_add (It *this, void *element);

/// it_add adds an element at the beginning of 'this's
It *it_add0 (It *this, void *element);

///
It *it_cat (It *this, It *another);

///
It *it_take (It *this, size_t n);

///
It *it_takef (It *this, int (*predicate)(void *e));

///
It *it_drop (It *this, size_t n);

///
It *it_dropf (It *this, int (*predicate)(void *e));

///
It *it_filter (It *this, int (*predicate)(void *e));

///
It *it_map (It *this, void *(*converter)(void *e));

/// it_map2 applies conv1 to the first element and conv2 to the others.
It *it_map2 (It *this, void *(*conv1)(void *e), void *(*conv2)(void *e));

/// Returns It<Tp>.
It *it_zip (It *it1, It *it2);

/// Returns It<Tp3>.
It *it_zip3 (It *it1, It *it2, It *it3);

///
It *it_reverse (It *this);

/// it_sort sorts 'this' calling 'arr_sort'.
It *it_sort (It *this, int (*greater)(void *e1, void *e2));

///
void it_each (It *this, void (*f)(void *e));

///
void it_each_ix (It *this, void (*f)(void *e, size_t ix));

///
size_t it_count (It *this);

///
int it_eq (It *it1, It *it2, int (*feq)(void *e1, void *e2));

///
int it_index (It *this, int (*predicate)(void *e));

///
int it_contains (It *this, int (*predicate)(void *e));

///
int it_last_index (It *this, int (*predicate)(void *e));

/// Returns the first element which satisfies 'predicate' or opt_empty().
Opt *it_find (It *this, int (*predicate)(void *e));

/// Returns the first element which satisfies 'predicate' or 'option'.
void *it_ofind (It *this, void *option, int (*predicate)(void *e));

/// Creates an array from 'this'.
Arr *it_to (It *this);

/// Creates an It from 'a'.
It *it_from (Arr *a);

/// Returns Tp<Arr, Arr> (dup, rest): 'dup' with duplicates values (only the
/// first case) and 'rest' with every element of 'this' without duplicates.
Tp *it_duplicates (It *this, int (feq)(void *e1, void *e2));

#endif


// Copyright 18-Oct-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Functions using external programs.

#ifndef DMC_EXT_H
  #define DMC_EXT_H

/// Calls "wget -q -O - 'url'" and returns the text read.
/// If the reading fails, it returns an empty string.
char *ext_wget (char *url);

/// Reads an URL.
/// Calls "node -e [script] 2>/dev/null" and returs the text read.
/// If the reading fails, it returns an empty string.
/// [script] is a script which run a node library called "puppeteer". This
/// library must be downloaded (npm install puppeteer).
///   url: URL to read. It must include protocol ("https://www.google.com").
char *ext_puppeteer (char *url);

/// Reads a text using GUI. It calls:
///   zenity --entry --title='title' --text='prompt' 2>/dev/null
/// The return removes starting and trailing spaces.
/// If user clicks on cancel, it returns an empty string.
/// It is posible set a default text adding in promp:
///   \" --entry-text \"[text_to_add]
char *ext_zenity_entry (char *title, char *prompt);

/// ext_zenity_msg shows a message box. It calls:
///   zenity --notification --window-icon='icon' --text='text' 2>/dev/null
/// 'icon' is one of gnome icon stock. For example: info, dialog-warning,
/// dialog-error, dialog-information, face-wink, etc
void ext_zenity_msg (char *icon, char *text);

/// ext_pdf generates a pdf file from a html text. It calls:
///   pdfPrinter -s %s -t %s 'options' 2>&1
///
///   tx_source  : Text html
///   file_target: Path of the new pdf file
///   options    : Options for pdfPrinter
void ext_pdf (
  char *tx_source,
  char *file_target,
  char *options
);

/// ext_zip compress source in target. It calls:
///   zip -q 'target' 'source' 2>&1
/// if 'target' already exists, source will be added to it. If you require a
/// fresh target file, you have to delete it previously.
///   source: can be a file or directory,
///   target: Zip file. If it is a relative path, it hangs on source parent.
void ext_zip (char *source, char *target);

/// ext_unzip uncompress source in target, It calls:
///   unzip -q 'source' -d 'target' 2>&1
///
///   source: Zip file.
///   target: A directory. It it does not exist, it is created.
void ext_unzip (char *source, char *target);

#endif
// Copyright 23-Apr-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Tuple of three values.

#ifndef DMC_TP3_H
  #define DMC_TP3_H

///
typedef struct tp3_Tp3 Tp3;

///
Tp3 *tp3_new(void *e1, void *e2, void *e3);

///
void *tp3_e1(Tp3 *this);

///
void *tp3_e2(Tp3 *this);

///
void *tp3_e3(Tp3 *this);

#endif
// Copyright 17-Oct-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Utilities for managing file paths.

#ifndef DMC_PATH_H
  #define DMC_PATH_H

#include "Opt.h"

/// Returns name and extension of path.
/// If path is "" or ends at ("/") it returns "".
char *path_name (char *path);

/// Returns the parent path of 'path'.
/// If 'path' is "/" or a string without '/', it returns an empty string.
char *path_parent (char *path);

/// Returns only extension of path. Extension is returned with point,
/// (e.g., ".", ".txt").
/// If path does not have extension it returns "".
char *path_extension (char *path);

/// Returns only name of path.
/// If path is "", ends at ("/"), or if file starts with point, it returns "".
char *path_only_name (char *path);

/// Concatenates paths. Variable argumens must finish with NULL.
char *path_cat (char *s, char *more, ...);

/// Returns Opt<char> Cannonical representation of 'path'.
///   - If some component of 'path' is not in file system, returns 'opt_empty'.
///   - Directories do not finish in '/'.
Opt *path_canonical (char *s);

#endif
// Copyright 18-Oct-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Utilities for managing dates.

#ifndef DMC_DATE_H
  #define DMC_DATE_H

#include <time.h>
#include "dmc/std.h"

/// Makes a new time_t.
///   this : New time_t.
///   year : Year with all digits.
///   month: Month in base 1 (1 to 12).
///   day  : Day in base 1 (1 to 31).
time_t date_new (int day, int month, int year);

/// Returns the current date.
time_t date_now (void);

/// Makes a date from a string is in format 'yyyymmdd'.
/// (month and day in base 1).
time_t date_from_str (char *date);

/// Makes a date from a string in format '[x]x/[x]x/[xxx]x'.
/// (month and day in base 1).
time_t date_from_iso (char *date);

/// Mmakes a date from a string in format '[x]x/[x]x/[xxx]x'.
/// (month and day in base 1).
time_t date_from_us (char *date);

/// Makes a date from a string is in format '[x]xSP[x]xSP[xxx]x'.
/// If 'data' is not valid, returns '0'.
time_t date_from_iso_sep (char *date, char sep);

/// Makes a date from a string in format '[x]xSP[x]xSP[xxx]x'.
/// If 'data' is not valid, returns '0'.
time_t date_from_us_sep (char *date, char sep);

///
int date_eq (time_t this, time_t another);

///
int date_cmp (time_t this, time_t another);

/// Returns the difference in days this - another.
int date_df (time_t this, time_t another);

///
time_t date_add (time_t this, int days);

///
int date_day (time_t this);

///
int date_month (time_t this);

///
int date_year (time_t this);

/// Formats a time_t. Format can be:
///   %a     The abbreviated name of the day of the week according to the
///          current locale.
///   %A     The full name of the day of the week according to the current
///          locale.
///   %b     The abbreviated month name according to the current locale.
///   %B     The full month name according to the current locale.
///   %c     The preferred date and time representation for the current
///          locale.
///   %C     The century number (year/100) as a 2-digit integer. (SU)
///   %d     The day of the month as a decimal number (range 01 to 31).
///   %D     Equivalent to %m/%d/%y.  (Yecch—for Americans only.  Americans
///          should note that in other countries %d/%m/%y is rather common.
///          This means that in international context this format is
///          ambiguous and should not be used.) (SU).
///   %e     Like %d, the day of the month as a decimal number, but a
///          leading zero is replaced by a space. (SU).
///   %F     Equivalent to %Y-%m-%d (the ISO 8601 date format). (C99).
///   %G     The ISO 8601 week-based year (see NOTES) with century as a
///          decimal number.  The 4-digit year corresponding to the ISO
///          week number (see %V).  This has the same format and value as
///          %Y, except that if the ISO week number belongs to the previous
///          or next year, that year is used instead. (TZ).
///   %g     Like %G, but without century, that is, with a 2-digit year
///          (00-99). (TZ).
///   %h     Equivalent to %b.  (SU).
///   %H     The hour as a decimal number using a 24-hour clock (range 00
///          to 23).
///   %I     The hour as a decimal number using a 12-hour clock (range 01
///          to 12).
///   %j     The day of the year as a decimal number (range 001 to 366).
///   %k     The hour (24-hour clock) as a decimal number (range 0 to 23);
///          single digits are preceded by a blank.  (See also %H.)  (TZ)
///   %l     The hour (12-hour clock) as a decimal number (range 1 to 12);
///          single digits are preceded by a blank.  (See also %I.)  (TZ)
///   %m     The month as a decimal number (range 01 to 12).
///   %M     The minute as a decimal number (range 00 to 59).
///   %n     A newline character. (SU)
///   %O     Modifier: use alternative format, see below. (SU)
///   %p     Either "AM" or "PM" according to the given time value, or the
///          corresponding strings for the current locale.  Noon is treated
///          as "PM" and midnight as "AM".
///   %P     Like %p but in lowercase: "am" or "pm" or a corresponding
///          string for the current locale. (GNU)
///   %r     The time in a.m. or p.m. notation.  In the POSIX locale this
///          is equivalent to %I:%M:%S %p.  (SU)
///   %R     The time in 24-hour notation (%H:%M).  (SU) For a version
///          including the seconds, see %T below.
///   %s     The number of seconds since the Epoch, 1970-01-01 00:00:00
///          +0000 (UTC). (TZ)
///   %S     The second as a decimal number (range 00 to 60).  (The range
///          is up to 60 to allow for occasional leap seconds).
///   %t     A tab character. (SU).
///   %T     The time in 24-hour notation (%H:%M:%S). (SU).
///   %u     The day of the week as a decimal, range 1 to 7, Monday being
///          1.  See also %w.  (SU)
///   %U     The week number of the current year as a decimal number, range
///          00 to 53, starting with the first Sunday as the first day of
///          week 01.  See also %V and %W.
///   %V     The ISO 8601 week number (see NOTES) of the current year as a
///          decimal number, range 01 to 53, where week 1 is the first week
///          that has at least 4 days in the new year.  See also %U and %W.
///          (SU).
///   %w     The day of the week as a decimal, range 0 to 6, Sunday being
///          0.  See also %u.
///   %W     The week number of the current year as a decimal number, range
///          00 to 53, starting with the first Monday as the first day of
///          week 01.
///   %x     The preferred date representation for the current locale
///          without the time.
///   %X     The preferred time representation for the current locale
///          without the date.
///   %y     The year as a decimal number without a century (range 00 to
///          99).
///   %Y     The year as a decimal number including the century.
///   %z     The +hhmm or -hhmm numeric timezone (that is, the hour and
///          minute offset from UTC). (SU).
///   %Z     The timezone name or abbreviation.
///   %%     A literal '%' character.
char *date_f (time_t this, char *template);

/// Returns a string in format 'yyyymmdd'.
char *date_to_str (time_t this);

/// Returns a string in format 'dd/mm/yyyy'.
char *date_to_iso (time_t this);

/// Returns a string in format 'mm/dd/yyyy'.
char *date_to_us (time_t this);

///
Js *date_to_js (time_t this);

///
time_t date_from_js (Js *js);

///
typedef struct timeval DateTm;

/// Returns the current time with microsconds of precission.
///   DateTm (struct timeval) has following fields:
///     time_t tv_sec - Seconds.
///     long int tv_usecs - Microseconds (0 - 999999).
DateTm *dateTm_now ();

/// Returns t1 - t2.
DateTm *dateTm_tdf (DateTm *t1, DateTm *t2);

/// Adds 'millis' milliseconds to 't'. 'millis' can be negative.
DateTm *dateTm_add (DateTm *t, int millis);

/// Returns t1 - t2 in milliseconds.
int dateTm_df (DateTm *t1, DateTm *t2);

#endif
// Copyright 16-Oct-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

///String utilities.

#ifndef DMC_STR_H
  #define DMC_STR_H

#include <stdarg.h>
#include <string.h>
#include "Opt.h"
#include "Arr.h"

/// Returns a copy of 'str'.
char *str_new(char *s);

/// Returns a string with the character 'ch'.
char *str_c(char ch);

/// Returns 'strcoll(s1, s2)'.
int str_cmp_locale(char *s1, char *s2);

/// Returns 'strcmp(s1, s2) > 0'.
int str_greater(char *s1, char *s2);

/// Returns 'strcoll(s1, s2) > 0'.
int str_greater_locale(char *s1, char *s2);

/// Returns 'true' if 'str1 == str2'.
int str_eq(char *str1, char *str2);

/// Indicates if 'str' starts with 'substr'.
int str_starts(char *str, char *substr);

/// Indicates if 'str' ends with 'substr'.
int str_ends(char *str, char *substr);

/// Returns first position of 'ch' in 'str' or -1 if 'ch' is missing.
int str_cindex(char *str, char ch);

/// Returns first position of 'ch' in 'str' from start or -1 if 'ch' is missing.
int str_cindex_from(char *str, char ch, int start);

/// Returns first position of 'substr' in 'str' or -1 if 'substr' is missing.
int str_index(char *str, char *substr);

/// Returns first position of 'substr' in 'str' from start or -1 if 'substr' is
/// missing.
int str_index_from(char *str, char *substr, int start);

/// Returns last position of 'ch' in 'str' or -1 if 'ch' is missing.
int str_last_cindex(char *str, char ch);

/// Returns last position of 'substr' in 'str' or -1 if 'substr' is missing.
int str_last_index(char *str, char *substr);

/// str_cat is a string concatenation.
/// Variable argumens must finish with NULL.
char *str_cat(char *s, ...);

/// Returns a substring.
///   - If 'begin' or 'end' are negatives, they are subtracted from 'strlen(s)'.
///   - If 'begin' or 'end' are out of bounds, function throws a RANGE exception.
char *str_sub(char *str, int begin, int end);

/// str_left is equals to 'str_sub(str, 0, end)'.
char *str_left(char *str, int end);

/// str_right is equals to 'str_sub(str, begin, strlen(s))'.
char *str_right(char *str, int begin);

/// Returns a new string removing spaces (ch <= ' ') at left.
char *str_ltrim(char *str);

/// Returns a new string removing spaces (ch <= ' ') at right.
char *str_rtrim(char *str);

/// Returns a new string removing spaces (ch <= ' ') at left and right.
char *str_trim(char *str);

/// Splits 'str' in an Arr[char].
/// For example (using ';' as separator):
///   "" -> [""]
///   ";" -> ["", ""]
///   "a;" -> [a, ""]
///   "a;bc" -> ["a", "bc"]
/// Returns an Arr<char>.
Arr *str_csplit(char *str, char sep);

/// str_csplit_trim is similar to <tt>str_csplit</tt> but trimming elements.
/// Returns an Arr<char>.
Arr *str_csplit_trim(char *str, char sep);

/// Splits 'str' in an Arr<char>.
/// For example (using ";" as separator):
///   "" -> [""]
///   ";" -> ["", ""]
///   "a;" -> [a, ""]
///   "a;bc" -> ["a", "bc"]
/// If 'sep' is "" return all runes of 'str'.
/// Returns an Arr<char>.
Arr *str_split(char *str, char *sep);

/// str_split_trim is similar to <tt>str_split</tt> but trimming elements.
/// Returns an Arr[char]
Arr *str_split_trim(char *str, char *sep);

/// Joins elements of 'a', separated by 'sep'.
/// 'a' is Arr<char>.
char *str_cjoin(Arr *a, char sep);

/// Joins elements of 'a', separated by 'sep'.
/// 'a' is Arr<char>.
char *str_join(Arr *a, char *sep);

/// Returns a new string replacing 'old' by 'new' in 's'.
char *str_creplace(char *s, char old, char new);

/// Returns a new string replacing 'old' by 'new' in 's'.
/// If 'old' is "", it does nothing.
char *str_replace(char *s, char *old, char *new);

/// Returns a string with format similar to 'vprintf'.
char *str_vf(char *format, va_list args);

/// Returns a string with format similar to 'printf'.
char *str_f(char *format, ...);

/// Returns utf8 caracter number or -1 if 's' is not a valid utf8 string.
int str_runes(char *s);

/// Reads next rune of 's'.
/// If there are no more runes or it fails, 'rune' is equals to "".
/// Example:
///   char *tx = "a text";
///   char *rune;
///   char *rest = str_next_rune(&rune, tx);
///   while (*rune) {
///     puts(rune);
///     rest = str_next_rune(&rune, rest);
///   }
char *str_next_rune(char **rune, char *s);

/// Codifies a string in Unicode. Returns an 'Opt_empty()' if there are errors.
///   return: Opt<unsigned>
Opt *str_to_unicode(char *s);

/// Decodifies a string from Unicode. Returns an 'Opt_empty()' if there are
/// errors.
///   return: Opt<char>
Opt *str_from_unicode(unsigned *u);

/// Decodifies a string from ISO-8859-1.
char *str_from_iso(char *s);

/// Returns 's' converted to uppercase. It is necessary set language previously.
/// For example:
///   sys_locale("es_ES.utf8");
///   char *s = str_to_upper_new("cañón");
///   puts(s);
/// This function can fail if s is not a valid utf-8 string.
char *str_to_upper (char *s);

/// Returns 's' converted to lowercase. It is necessary set language previously.
/// For example:
///   sys_locale("es_ES.utf8");
///   char *s = str_to_lower_new("cañón");
///   puts(s);
/// This function can fail if s is not a valid utf-8 string.
char *str_to_lower (char *s);

/// Replaces " by \" and \ by \\ and insert the result inside quotes.
char *str_to_escape (char *s);

/// Restores the string escaped with 'escape'. If 's' does not come from
/// 'escape' the result is indefined.
char *str_from_escape (char *s);

#endif
// Copyright 16-Oct-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Map structure.

#ifndef DMC_MAP_H
  #define DMC_MAP_H

#include "dmc/Arr.h"
#include "dmc/Opt.h"

///
typedef struct map_Map Map;

/// Initializates a map. Map can be cast to Arr<Kv>.
Map *map_new(void);

///
int map_size(Map *this);

/// Puts 'value' with key 'key'. If key already exists its value
/// is changed.
///   this : The map.
///   key  : Entry key.
///   value: New value
void map_put(Map *this, char *key, void *value);

/// Returns 1 if 'this' has key 'key'.
int map_has_key(Map *this, char *key);

/// Returns the value pointed by key or 'opt_empty' if 'key' does not exist.
Opt *map_get(Map *this, char *key);

/// Removes value with key 'key' or does nothing if 'key' does not exist.
void map_remove(Map *this, char *key);

/// Returns keys of this in a Arr<char>.
Arr *map_keys(Map *this);

/// Equals to (Arr *)this
/// Returns an Arr[Kv]
Arr *map_kvs(Map *this);

/// Sorts 'this' from keys.
void map_sort(Map *this);

/// Sorts 'this' in locale from keys.
void map_sort_locale(Map *this);

/// Creates an iterator over 'this'.
///   return: It<Kv>
It *map_to_it(Map *this);

/// Creates a Map from 'it'
///   it: It[Kv]
Map *map_from_it(It *it);

/// Returns a Js from a value of 'this'.
///   to: Value converter.
Js *map_to_js(Map *this, Js *(*to)(void *e));

/// Parses a Js to a value of 'this'.
///   from: Value converter.
Map *map_from_js(Js *js, void *(*from)(Js *jse));

#endif
// Copyright 15-Oct-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Definitions.

#ifndef DMC_DEFS_H
  #define DMC_DEFS_H

typedef struct js_Js Js;

typedef struct schd_Task SchdTask;

///
#define MALLOC(type) (type *)GC_MALLOC(sizeof(type))

///
#define ATOMIC(size) GC_MALLOC_ATOMIC(size)

///
#define REPEAT(n) { \
  int __i = (n) + 1; \
  while (--__i) {

///
#define _REPEAT }}

///
#define RANGE0(i, end) { \
  int __end = end; \
  int i = -1; \
  while (++i < __end) {

///
#define RANGE(i, begin, end) { \
  int __end = end; \
  int i = (begin) - 1; \
  while (++i < __end) {

///
#define _RANGE }}

/// Iterates over an Iarr. See EACH.
#define IEACH(a, n) { \
  Iarr *__a = a; \
  int *__p = iarr_start(__a); \
  int *__pend = iarr_end(__a); \
  int n; \
  while(__p < __pend) { \
    n = *__p++;

/// Iterates over an Iarr. See EACH_IX.
#define IEACH_IX(a, n, ix) { \
  Iarr *__a = a; \
  int *__p = iarr_start(__a); \
  int *__pend = iarr_end(__a); \
  int n; \
  int ix = -1; \
  while(__p < __pend) { \
    ++ix; \
    n = *__p++;

/// Iterates over an Darr. See EACH.
#define DEACH(a, n) { \
  Darr *__a = a; \
  double *__p = darr_start(__a); \
  double *__pend = darr_end(__a); \
  double n; \
  while(__p < __pend) { \
    n = *__p++;

/// Iterates over an Darr. See EACH_IX
#define DEACH_IX(a, n, ix) { \
  Darr *__a = a; \
  double *__p = darr_start(__a); \
  double *__pend = darr_end(__a); \
  double n; \
  int ix = -1; \
  while(__p < __pend) { \
    ++ix; \
    n = *__p++;

/// Iterates over an 'Arr'. You can access to the 'element' index with _i.
/// For example:
///   EACH(a, char, s) {
///     printf("[%d] -> %s\n", _i, s);
///   } _EACH
///
///   a      : An Arr *.
///   type   : Element type without pointer sign (*).
///   element: An element of type 'type'.
#define EACH(a, type, element) { \
  Arr *__arr = (Arr *)a; \
  size_t __size = arr_size(__arr); \
  size_t _i; \
  type *element; \
  for (_i = 0; _i < __size; ++_i) { \
    element = arr_get(__arr, _i);

/// Iterates over an 'Arr'. You can access to the 'element' index with i.
/// For example:
///   EACH(a, char, s, i) {
///     printf("[%d] -> %s\n", i, s);
///   } _EACH
///
///   a      : An Arr *.
///   type   : Element type without pointer sign (*).
///   e: An element of type 'type'.
///   i: Element index.
#define EACH_IX(a, type, e, i) { \
  Arr *__a = (Arr *)a; \
  void **__p = arr_start(__a); \
  void **__pend = arr_end(__a); \
  type *e; \
  int i = -1; \
  while(__p < __pend) { \
    ++i; \
    e = *__p++;

/// Iterates over an 'It'. You can access to the 'element' index with _i.
/// For example:
///   EACH(it, char, s) {
///     printf("[%d] -> %s\n", _i, s);
///   } _EACH
///
///   a      : An It *.
///   type   : Element type without pointer sign (*).
///   element: An element of type 'type'.
#define EACHI(it, type, element) { \
  It *__it = (It *)it; \
  int _i = -1; \
  type *element; \
  while (it_has_next(__it)) { \
    ++_i; \
    element = it_next(__it);

/// Iterates over a 'List'.
/// For example:
///   EACHL(list, char, s) {
///     printf("%s\n", s);
///   } _EACH
///
///   list   : An List *,
///   type   : Element type without pointer sign (*),
///   element: An element of type 'type',
#define EACHL(list, type, element) { \
  List *_EACHL_list = (List *)list; \
  type *element; \
  while (!list_empty(_EACHL_list)) { \
    element = list_head(_EACHL_list); \
    _EACHL_list = list_tail(_EACHL_list);

/// Iterates over an 'Arr' in reverse order. You can access to the 'element'
/// index with _i.
/// For example:
///   EACHR(a, char, s) {
///     printf("[%d] -> %s\n", _i, s);
///   } _EACH
///
///   a      : An Arr *.
///   type   : Element type without pointer sign (*).
///   element: An element of type 'a'.
#define EACHR(a, type, element) { \
  Arr *__arr = (Arr *)a; \
  size_t _i = arr_size(__arr); \
  type *element; \
  while (_i) { \
    element = arr_get(__arr, --_i);

/// Finalizes an EACHL, EACH or a EACHR.
#define _EACH }}

///
typedef void *(*FCOPY)(void *);

///
typedef int (*FCMP)(void *, void *);

///
typedef void (*FPROC)(void *);

///
typedef int (*FPRED)(void *);

///
typedef Js *(*FTO)(void *);

///
typedef void *(*FFROM)(Js *);

///
typedef void (*FLOOP)(void *, SchdTask *);

/// Macros to manage exceptions. Example:
///   TRY
///     ...
///   CATCH (e)
///     puts(exc_msg(e));
///   _TRY
/// NOTE: CATCH block must return 'void'
#define TRY { \
  jmp_buf *_TRY_buf = MALLOC(jmp_buf); \
  exc_add(_TRY_buf); \
  if (!setjmp(*_TRY_buf)) { \

/// See TRY.
#define CATCH(e) ;exc_remove();} else { Exc *e = exc_get();

/// See>TRY.
#define _TRY ;exc_remove();}}

/// Example:
///   THROW(exc_io_t) "Working directory not found: %s", strerror(errno) _THROW
#define THROW(type) exc_throw(type, str_f(

///
#define _THROW ), __FILE__, (char *)__func__, __LINE__);

/// Example:
///   EXC_GENERIC("Fail")
#define EXC_GENERIC(msg) \
  THROW(exc_generic_t) msg _THROW

/// Throw a range exception if v < 'min' or v > 'max'.
/// Example:
///   EXC_RANGE(v, 0, 23) // -1 and 24 throw exeption.
#define EXC_RANGE(value, min, max) { \
    int __v = value; \
    if (__v < (min) || __v > (max)) \
      THROW(exc_range_t) exc_range((min), (max), __v) _THROW \
  }

/// Example:
///   EXC_ILLEGAL_ARGUMENT("Fail", "a value", "another value")
#define EXC_ILLEGAL_ARGUMENT(msg, expected, actual) \
  THROW(exc_illegal_argument_t) \
    exc_illegal_argument(msg, expected, actual) \
  _THROW

/// Example:
///   EXC_ILLEGAL_STATE("Fail")
#define EXC_ILLEGAL_STATE(msg) \
  THROW(exc_illegal_state_t) exc_illegal_state(msg) _THROW

/// Example:
///   EXC_IO("Fail")
#define EXC_IO(msg) \
  THROW(exc_io_t) exc_io(msg) _THROW

/// Reads a 'field' of 'map'. If 'field' is not found produce an
/// ILLEGAL_ARGUMENT exception, otherwise returns its value in 'type var'
/// using 'fun'.
/// Examples:
///   CGI_GET(int, index, js_ri, m)
///   CGI_GET(char *, value, js_rs, m)
///   CGI_GET(Arr *, values, js_ra, m)
///
///   type: type of var.
///   var : name of variable.
///   fun : function to pass 'Js' to 'type'.
///   map : A Map<Js>.
#define CGI_GET(type, var, fun, map) \
  type var; \
  { \
    Opt *js = map_get(map, #var); \
    if (opt_is_empty(js))  \
      EXC_ILLEGAL_ARGUMENT(#var, "Map key", "Key not found") \
    var = fun(opt_get(js)); \
  }

/// Calls CGI_GET with 'var' as 'int'.
#define CGI_GET_BOOL(var, map) \
  CGI_GET(int, var, js_rb, map)

/// Calls CGI_GET with 'var' as 'int'.
#define CGI_GET_INT(var, map) \
  CGI_GET(int, var, js_ri, map)

/// Calls CGI_GET with 'var' as 'double'.
#define CGI_GET_DOUBLE(var, map) \
  CGI_GET(double, var, js_rd, map)

/// Calls CGI_GET with 'var' as 'char *'.
#define CGI_GET_STR(var, map) \
  CGI_GET(char *, var, js_rs, map)

/// Calls CGI_GET with 'var' as 'Arr<Js>'.
#define CGI_GET_ARR(var, map) \
  CGI_GET(Arr *, var, js_ra, map)

/// Calls CGI_GET with 'var' as 'Map<Js>'.
#define CGI_GET_MAP(var, map) \
  CGI_GET(Map *, var, js_ro, map)

#endif
// Copyright 16-Oct-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Utilities for managing files.

#ifndef DMC_FILE_H
  #define DMC_FILE_H

#include <time.h>
#include "dmc/Arr.h"
#include "dmc/Bytes.h"

///
typedef struct file_FileLck FileLck;

/// Returns a new file path whose template is:
/// /tmp/'prefix'xxxxxxxxxx. Where xxxxxxxxxx is an aleatory sequence of
/// digits.
char *file_tmp (char *prefix);

/// Returns a new file path whose template is:
/// 'dir'/'prefix'xxxxxxxxxx. Where xxxxxxxxxx is an aleatory sequence of
/// digits.
char *file_tmp_in (char *dir, char *prefix);

/// Returns the working directory
char *file_cwd (void);

/// Sets the working directory
void file_cd (char *path);

/// Makes a directory with 0755 permissions.
/// If parent directory does not exist it creates it.
/// If 'path' already exists it does nothing.
void file_mkdir (char *path);

/// fReturns an Arr[char] with names of files and directories existing in
/// 'path'.
/// Values '.' and '..' are not in the return.
Arr *file_dir (char *path);

/// Deletes file or directory named 'path' although it is a not empty directory.
/// If 'path' does not exists it does nothing.
void file_del (char *path);

/// Renames 'old_path' to 'new_path'.
void file_rename (char *old_path, char *new_path);

/// Makes a symbol link from 'new_path' to 'old_path'.
void file_link (char *old_path, char *new_path);

/// file_exits returns true if 'path' exists in the file system.
int file_exists (char *path);

/// Returns true if file is a directory
int file_is_directory (char *path);

/// Returns information of 'path'. If path does no exist or another error
/// happens, this function produces a exception.
/// Some fields to check in 'struct stat' are:
///   mode_t st_mode  - It can be tested with S_ISDIR(), S_ISREG or S_ISLNK
///                     among others. This macros are defined in sys/stat.h
///   uid_t st_uid    - User id
///   gid_t st_gid    - Group id
///   off_t st_size   - File size
///   time_t st_atime - Last access to file in seconds.
///   time_t st_mtime - Last file modification in seconds.
struct stat *file_info (char *path);

/// Returns the size of 'path'.
int file_size(char *path);

/// Returns the last access in seconds.
time_t file_modified(char *path);

/// reads data from 'path', including ends of line.
/// This function opens, reads and closes file.
char *file_read (char *path);

/// Writes 'data' on 'path'.
/// This function opens, writes and closes file.
void file_write (char *path, char *text);

/// Appends 'data' on 'path'.
/// This function opens, writes and closes file.
void file_append (char *path, char *text);

/// Binary copy source to target.
void file_copy (char *source_path, char *target_path);

/// Opens a file to read with file_read_line or file_read_bin.
/// It returns a FileLck object which will be freed when close is called.
FileLck *file_ropen (char *path);

/// Opens a file to write with file_write_line or file_write_bin.
/// It returns a FileLck object which will be freed when close is called.
FileLck *file_wopen (char *path);

/// Opens a file to append with file_write_line or file_write_bin.
/// It returns a FileLck object which will be freed when close is called..
FileLck *file_aopen (char *path);

/// Reads a text file opened with file_ropen.
/// It does not delete ends of line.
/// When reading is finished, returns a blank string.
char *file_read_line (FileLck *lck);

/// Writes a text file opened with file_wopen or file_aopen.
void file_write_text (FileLck *lck, char *text);

/// Reads a binary file opened with file_ropen.
/// When reading is finished, returns an empty Bytes.
Bytes *file_read_bin_buf (FileLck *lck, int buffer);

/// file_read_bin is the same as 'file_read_bin_bf' using a buffer of 8192.
Bytes *file_read_bin (FileLck *lck);

/// Writes a binary file opened with file_wopen.
/// Returns 0 if there is no error.
void file_write_bin (FileLck *lck, Bytes *bs);

/// Closes a file open with file_ropen, file_wopen or file_aopen.
void file_close (FileLck *lck);

#endif
// Copyright 16-Oct-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Strings constructor.

#ifndef DMC_BUF_H
  #define DMC_BUF_H

///
typedef struct buf_Buf Buf;

/// Initializes a 'buf' with size 150.
Buf *buf_new(void);

/// Initializes a 'buf'.
Buf *buf_bf_new(int buffer_size);

/// Returns the length of the enveloped string.
int buf_len(Buf *this);

/// Returns a reference to the string wrapped. Return is intended to
/// be not modified.
char *buf_str(Buf *this);

/// Adds 'length bytes of 'data' to 'buf'.
/// 'length' must be less or equals to 'strlen(data)'.
/// It is not necessary that 'data' be a null-terminated string, but it must
/// no have characters \0
void buf_add_buf(Buf *this, char *data, int length);

/// Adds 'data' to 'buf'.
void buf_add(Buf *this, char *data);

/// Adds a character.
void buf_cadd(Buf *this, char data);

/// Returns a copy of the enveloped string.
char *buf_to_str(Buf *this);

/// Resets buffer (but does not reduce its size).
void buf_reset(Buf *this);

#endif
// Copyright 22-Apr-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Option.

#ifndef DMC_OPT_H
  #define DMC_OPT_H

typedef struct opt_Opt Opt;

/// Creates a full option.
Opt *opt_new (void *value);

/// Creates an empty option.
Opt *opt_empty (void);

/// Returns 1 if option has no value.
int opt_is_empty (Opt *this);

/// Returns 1 if option has value.
int opt_is_full (Opt *this);

/// Throws a illegal_state_exception_t if 'this' is empty.
void *opt_get (Opt *this);

/// Throws a illegal_state_exception_t if 'this' is empty with 'msg' as message.
void *opt_eget (Opt *this, char *msg);

/// Returns value if 'this' is empty.
void *opt_oget (Opt *this, void *value);

/// Returns NULL if 'this' is empty.
void *opt_nget (Opt *this);

#endif
// Copyright 17-Oct-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Utilities for managing json strings.

#ifndef DMC_JS_H
  #define DMC_JS_H

#include "dmc/Arr.h"
#include "dmc/Map.h"

/// 'Js' is an alias of 'char'.
typedef struct js_Js Js;

/// Returns '1' if json is "null" or '0' in another case.
int js_is_null (Js *json);

/// Read boolean.
int js_rb (Js *json);

/// Read int.
int js_ri (Js *json);

/// Read long.
long js_rl (Js *json);

/// Read double.
double js_rd (Js *json);

/// Read string.
char *js_rs (Js *json);

/// Read array in an Arr<Js>.
Arr *js_ra (Js *json);

/// Read object in a Map<js>.
Map *js_ro (Js *json);

/// Write a null value.
Js *js_wn(void);

/// Write a boolean value.
Js *js_wb(int value);

/// Write an int value.
Js *js_wi(int n);

/// Write an long value.
Js *js_wl(long n);

/// Write a double value with a maximum of 9 decimal positions.
Js *js_wd(double n);

/// Write a string.
Js *js_ws(char *s);

/// Write an Arr<Js>.
Js *js_wa(Arr *a);

/// Write a Map<Js>.
Js *js_wo(Map *m);

#endif
// Copyright 15-Oct-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Array of ints.

#ifndef DMC_IARR_H
  #define DMC_IARR_H

#include "Js.h"

///
typedef struct iarr_Iarr Iarr;

///
Iarr *iarr_new(void);

/// buffer must be > 0
Iarr *iarr_bf_new(int buffer);

/// Creates a new array from a C array. For example:
///   Iarr *a = iarr_new_c(3, (int[]){2, 0, 4});
/// If 'size' is less than C array length, result is ok (only will be
/// used 'size' first elements); but if 'size' is greater, the result is
/// undetermined.
Iarr *iarr_new_c (int size, int *es);

/// Returns a new Iarr with elements from 0 to 'ix' (exclusive),
Iarr *iarr_left(Iarr *this, int ix);

/// Returns a new Iarr with elements from 'ix' (inclusive) to end of 'this'.
Iarr *iarr_right(Iarr *this, int ix);

/// Returns a new Iarr with elements from 'begin' (inclusive) to
/// to 'end' (exclusive),
Iarr *iarr_sub(Iarr *this, int begin, int end);

///
Iarr *iarr_copy(Iarr *this);

///
int iarr_size(Iarr *this);

///
int iarr_eq(Iarr *this, Iarr *other);

/// If ix is < 0 then is changed to 'iarr_size - ix'.
int iarr_get(Iarr *this, int ix);

///
int *iarr_start(Iarr *this);

///
int *iarr_end(Iarr *this);

///
void iarr_push(Iarr *this, int e);

/// If ix is < 0 then is changed to 'iarr_size - ix'.
void iarr_set(Iarr *this, int ix, int e);

/// If ix is < 0 then is changed to 'iarr_size - ix'.
void iarr_insert(Iarr *this, int ix, int e);

/// If ix is < 0 then is changed to 'iarr_size - ix'.
void iarr_remove(Iarr *this, int ix);

///
void iarr_cat(Iarr *this, Iarr *other);

/// If ix is < 0 then is changed to 'iarr_size - ix'.
void iarr_insert_arr(Iarr *this, int ix, Iarr *other);

/// If begin or end are < 0 then is changed to 'iarr_size - itsValue'.
void iarr_remove_range(Iarr *this, int begin, int end);

/// Removes every element of 'this'. Buffer size is equals to 15.
void iarr_clear (Iarr *this);

/// Removes every element of 'this'.
void iarr_bf_clear (Iarr *this, int buffer);

///
void iarr_reverse(Iarr *this);

///
void iarr_sort(Iarr *this);

///
Js *iarr_to_js(Iarr *this);

///
Iarr *iarr_from_js(Js *js);

#endif
// Copyright 15-Oct-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Standard files of dmc.

#ifndef DMC_STD_H
  #define DMC_STD_H

#include <stdio.h>
#include <stdlib.h>
#include <gc.h>

#include "DEFS.h"
#include "Tp.h"
#include "Kv.h"
#include "Tp3.h"
#include "Exc.h"
#include "Opt.h"
#include "Bytes.h"
#include "Arr.h"
#include "It.h"
#include "Map.h"
#include "Buf.h"
#include "str.h"
#include "sys.h"
#include "path.h"
#include "file.h"
#include "Js.h"

#endif
// Copyright 1-Jun-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// List (immutable) structure.

#ifndef DM_LIST_H
  #define DM_LIST_H

///
typedef struct list_List List;
typedef struct opt_Opt Opt;
typedef struct it_It It;
typedef struct arr_Arr Arr;
typedef struct js_Js Js;

/// Creates a new List.
List *list_new(void);

/// Returns the number of elements. (O(n) operation).
int list_count(List *this);

/// Returns every element of 'this' less the first one. If "this" is
/// empty, throws an exception.
List *list_tail(List *this);

/// Returns the first element of 'this'. If "this" is empty.
/// throws an exception.
void *list_head (List *this);

/// Returns the element in 'ix' position -- head is in position 0.
/// (O(n) operation).
Opt *list_get (List *this, int ix);

/// Returns '1' if 'this' is empty.
int list_empty(List *this);

/// Adds 'o' at head. 'o' must be not NULL.
List *list_cons(List *this, void *o);

/// Returns 'this + l'.
List *list_cat(List *this, List *l);

/// list_reverse returns this in reverse order.
List *list_reverse(List *this);

/// list_to_it returns an iterator from top to bottom.
It *list_to_it (List *this);

/// list_from_it return a List with elements of 'it' in reverse order.
List *list_from_it (It *it);

/// list_to_arr returns an Arr with 'this' elements.
Arr *list_to_arr (List *this);

/// list_from_arr returns a list with 'a' elements.
List *list_from_arr (Arr *a);

/// list_to_json returns a serialization of 'this' using 'to' to.
/// convert elements.
Js *list_to_js(List *this, Js *(*to)(void *));

/// list_from_json restores a serialized List using 'from' to convert elements.
List *list_from_js(Js *js, void *(*from)(Js *));

#endif
// Copyright 15-Oct-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Utilities for managing globals values.
/// A way to initializes system is:
///   sys_init("app_name");
///   sys_locale("es_ES.utf8");

#ifndef DMC_SYS_H
  #define DMC_SYS_H

#include "dmc/Opt.h"

/// Initializates a normal program and call 'rnd_init()'.
/// After call this function 'sys_home' and 'sys_user' are available.
/// It creates the user directory in "~/.dmCApp/" + 'path'.
void sys_init (char *path);

///
char *sys_home (void);

///
char *sys_uname (void);

///
char *sys_udir (void);

/// Sets LC_ALL, for example:
///   sys_set_locale("es_ES.utf8")
void sys_set_locale (char *language);

/// Returns the current locale.
char *sys_locale (void);

/// Executes 'command', redirecting stderr to stdout, and returns its standard
/// output. If command fails, function returns an empty Opt.
///   return: Opt<char>.
Opt *sys_cmd(char *command);

/// Stops the current thread.
void sys_sleep (int millis);

#endif
// Copyright 01-May-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Inet server.

#ifndef DMC_ISERVER_H
  #define DMC_ISERVER_H

#include "dmc/std.h"

/// Request read by Iserver.
typedef struct iserver_Rq IserverRq;

/// Returns a error message or an empty string if everithing was well.
char *iserverRq_error (IserverRq *this);

/// Returns Opt<char>. If no request was received, returns 'opt_emtpy'.
/// If 'iserverRq_error' returns an error, throws an ILLEGAL_STATE exception.
Opt *iserverRq_msg (IserverRq *this);

/// Retuns the direction (IPv4) of connected server. Throws an ILLEGAL_STATE
/// exception if no request was read.
/// For being sure about receiving request, 'iserverRq_msg' should be tested.
char *iserverRq_host (IserverRq *this);

/// Writes response in 'this' and close it.
/// Returns an error message or an empty string if everithing was well.
char *iserverRq_write (IserverRq *this, char *response);

///
typedef struct iserver_Iserver Iserver;

///
Iserver *iserver_new (int port);

/// Read text in a not blocking way.
IserverRq *iserver_read (Iserver *this);

///
void iserver_close (Iserver *this);

#endif
// Copyright 18-Oct-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Decimal number and numeric utilities.

#ifndef DMC_DEC_H
  #define DMC_DEC_H

#include "Js.h"

///
typedef struct dec_Dec Dec;

/// Makes a new Dec. It requires link with -lm.
///   n    : Number which will be rounded to 'scale'.
///   scale: Decimal positions. Maximum scale is 10.
Dec *dec_new(double n, int scale);

///
char *dec_to_str(Dec *this);

/// Returns the rounded double value of 'this'.
double dec_n(Dec *this);

/// Returns the scale of 'this'.
int dec_scale(Dec *this);

/// Return has an error gap proportional to digits of 'd1' and 'd2'.
int dec_eq(double d1, double d2);

/// Returns true if d1 == d2 with an error margin of +- gap.
int dec_eq_gap(double d1, double d2, double gap);

/// Returns true if d1 == d2 with an error margin of +- gap.
int dec_eqf_gap(float d1, float d2, float gap);

/// Returns 'true' if all characters of 's' are digits.
/// ("" returns 'true').
int dec_digits(const char *s);

/// Returns a number without thousand separators and
/// with decimal point.
char *dec_regularize_iso(char *s);

/// Returns a number without thousand separators and with
/// decimal point.
char *dec_regularize_us(char *s);

/// Returns 'true' if "s" is a regularized number. If is "" returns 'true'.
/// "xxx.", "." or ".xxx" also return 'true'.
int dec_number(char *s);

///
Js *dec_to_js(Dec *this);

///
Dec *dec_from_js(Js *js);

#endif
// Copyright 23-Apr-2019 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Pair of two values.

#ifndef DMC_TP_H
  #define DMC_TP_H

///
typedef struct tp_Tp Tp;

///
Tp *tp_new(void *e1, void *e2);

///
void *tp_e1(Tp *this);

///
void *tp_e2(Tp *this);

#endif
// Copyright 17-Oct-2018 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// B64 encoder - decoder

#ifndef DMC_B64_H
  #define DMC_B64_H

#include "dmc/Bytes.h"

///
char *b64_decode(char *b64);

///
Bytes *b64_decode_bytes(char *b64);

///
char *b64_encode(char *s);

///
char *b64_encode_bytes(Bytes *bs);

#endif
