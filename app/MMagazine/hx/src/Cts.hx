// Copyright 31-Dec-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import dm.Client;
import dm.Ui.Q;
import I18n._;

/// Constants.
class Cts {
  /// Application name.
  public static final appName = "MMagazine";
  /// Application version.
  public static var version(default, null) = "202111";
  /// Page foot.
  public static final foot = Q("table")
    .klass("main")
    .add(Q("tr")
      .add(Q("td")
        .add(Q("hr"))))
    .add(Q("tr")
      .add(Q("td")
        .style("text-align: right;color:#808080;font-size:x-small;")
        .html('- © ºDeme. ${Cts.appName} (${Cts.version}) -')))
  ;
  /// Application client.
  public static final client = new Client(true, Cts.appName, () -> {
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
    "rgba(0, 160, 0)",
  ];
}
