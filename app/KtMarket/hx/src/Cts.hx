// Copyright 11-May-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

using StringTools;

import dm.Client;
import dm.Dec;
import dm.Ui.Q;
import I18n._;

/// Constants and global functions.
class Cts {
  /// Application name.
  public static final appName = "KtMarket";
  /// Application version.
  public static final appVersion = "202205";
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
  public static final bet = 10000.0;
  /// Investor initial capital.
  public static final initialCapital = 100000.0;
  /// Minimun cash to bet
  public static final minToBet = 11000.0;
  /// Gol multiplicator for coughts
  public static final noLostMultiplicator = 1.05;
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
  /// Number of quotes for accounting charts.
  public static final accountingQuotes = 250;


  /// Standar messsage for server fails.
  public static final okMsg = _("Operation successfully done.");
  /// Standar messsage for server fails.
  public static final failMsg = _("Operation failed.\nSee log.");
  /// ToBuy references colors (one by investor)
  public static final toBuyColors = [
    "rgba(160, 0, 0)",
    "rgba(224, 160, 0)",
    "rgba(240, 224, 0)",
  ];
  /// ToSell references colors (one by investor)
  public static final toSellColors = [
    "rgba(0, 0, 160)",
    "rgba(0, 160, 224)",
    "rgba(0, 224, 0)",
  ];

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
}
