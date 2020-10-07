// Copyright 15-Aug-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

using StringTools;

import dm.Client;
import dm.Dec;
import dm.Ui.Q;
import I18n._;

/// Constants and global functions.
class Cts {
  /// Application name.
  public static final appName = "MultiMarket";
  /// Application version.
  public static final appVersion = "202008";
  /// Page foot.
  public static final foot = Q("table")
    .klass("main")
    .add(Q("tr")
      .add(Q("td")
        .add(Q("hr"))))
    .add(Q("tr")
      .add(Q("td")
        .style("text-align: right;color:#808080;font-size:x-small;")
        .html('- © ºDeme. ${appName} (${appVersion}) -')))
  ;
  /// Application client.
  public static final client = new Client(true, appName, () -> {
    final wg = Q("div");
    MsgPage.mk(wg, appName, _("Session is expired."));
    Q("@body")
      .removeAll()
      .add(wg)
      .add(foot)
    ;
  });
  /// Ammount to make a buy.
  public static final bet = 10000;
  /// Investor initial capital.
  public static final initialCapital = 100000;
  /// Extern program name.
  public static final wget = "Wget";
  /// Extern program name.
  public static final puppeteer = "Puppeteer";
  /// Server state.
  public static final serverStopped = 0;
  /// Server state.
  public static final serverActive = 1;
  /// Server state.
  public static final serverSelected = 2;
  /// Standar messsage for server fails.
  public static final okMsg = _("Operation successfully done.");
  /// Standar messsage for server fails.
  public static final failMsg = _("Operation failed.\nSee log.");

  /// Format for numeric flea parameters.
  ///   v: Value
  ///   f: Format. Expected:
  ///     0 - Integer
  ///     4, 6 - Percentage.
  ///     Other - Normal number with 'other' decimals positions.
  public static function nformat (v: Float, f: Int): String {
    return f == 4 || f == 6
      ? Dec.toIso(v * 100, f - 2) + "%"
      : Dec.toIso(v, f)
    ;
  }

  /// Format for integers < 100
  ///   v: Value
  public static function format00(n: Int): String {
    if (n < 10) return "0" + Std.string(n);
    return Std.string(n);
  }
}
