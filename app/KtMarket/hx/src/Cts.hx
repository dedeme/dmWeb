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
  /// Decimals number of flea parameter.
//  public static final paramDecs = 6;
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
  /// Number of quotes for accounting charts.
  public static final accountingQuotes = 250;

  /// Minimum value to analyze by investors
//  public static final rangesMin = 12; // id 120000 -> parameter 0.12
	/// Group of 10000 to analyze
//	public static final rangesGroups = 10; // from 120000 inclusive to 220000 exclusive [0.12-0.22)
	/// Number of model per group
//	public static final rangesGroupNumber = 10000;
	/// Medium value to autamate investors creation.
//	public static final rangesMedium = 17;
  /// Returns the divisor value to convert a model id in a parameter.
//  public static final rangesToParam = 1000000; // id = 134567 -> range = id / 1000000 = 0.134567


  /// Investors number
//  public static final qlevels = 3;


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
