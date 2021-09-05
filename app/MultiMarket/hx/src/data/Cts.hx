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
  /// Decimals number of flea parameter.
  public static final paramDecs = 6;
  /// Ammount to make a buy.
  public static final bet = 10000.0;
  /// Investor initial capital.
  public static final initialCapital = 100000.0;
  /// Minimun cash to bet
  public static final minToBet = 11000.0;
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
  /// Minimum value to analyze
  public static final rangesMin = 0.1;
  /// Maximum value to analyze (exclusive)
  public static final rangesMax = 0.2;
  /// Medium value to autamate fleas creation.
  public static final rangesMedium = 0.15;

  /// Managers number
  public static final managersNumber = 3;
  /// Values of "jumper" from each investor. Used in daily charts of Companies.
  /// 'true': The investor is type Jump
  /// 'false: The investor is not type Jump.
  public static final managerJumpers = [true, true, false];
  /// Months to change parameters for each investor.
  public static final changeMonths = [
    _("January"),
    _("September"),
    _("May")
  ];
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

  /// Format for integers < 100
  ///   v: Value
  public static function format00(n: Int): String {
    if (n < 10) return "0" + Std.string(n);
    return Std.string(n);
  }

  /// Return qs['i'] if it is > 0.
  /// Otherwise return the last previous valid quote or 'def' if every previous
  /// quote is <= 0.
  public static function validQuote(qs: Array<Float>, i: Int, def: Float) {
    var q = qs[i];
    if (q > 0) return q;
    --i;
    while (i >= 0) {
      q = qs[i];
      if (q > 0) return q;
      --i;
    }
    return def;
  }
}
