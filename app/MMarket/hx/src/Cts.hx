// Copyright 15-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

using StringTools;

import dm.Client;
import dm.Ui.Q;
import dm.Dec;
import pgs.MsgPg;
import I18n._;
import dm.Opt;

/// Constants.
class Cts {
  /// Application name.
  public static final appName = "MMarket";
  /// Application version.
  public static final version = "202201";
  /// Page foot.
  public static final foot = Q("table")
    .klass("main")
    .add(Q("tr")
      .add(Q("td")
        .add(Q("hr"))))
    .add(Q("tr")
      .add(Q("td")
        .style("text-align: right;color:#808080;font-size:x-small;")
        .html('- © ºDeme. ${appName} (${version}) -')))
  ;
  /// Application client.
  public static final client = new Client(true, appName, () -> {
    final wg = Q("div");
    new MsgPg(wg, _("Session is expired.")).show();
    Q("@body")
      .removeAll()
      .add(wg)
      .add(foot)
    ;
  });
  /// Lang key
  public static final langKey = Cts.appName + "_lang";
  /// Model initial capital for each cycle
  public static final initialCapital = 100000.0;
  /// Historic simulation ratio
  public static final assetsRatio = 0.35;
  /// Maximum assets to calculate 'cts_assets_ratio' (Currency)
  public static final maxAssetsRatio = initialCapital * 3;
  /// Average of simulation profits ratio
  public static final profitsAvgRatio = 0.65;
  /// Maximum average to calculate 'cts_profits_avg_ratio' (ratio)
  public static final maxProfitsAvgRatio = 3;

}
