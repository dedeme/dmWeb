// Copyright 01-Dic-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import dm.Client;
import dm.Ui.Q;
import I18n._;

/// Constants.
class Cts {
  /// Application name.
  public static final appName = "KtWeb:FMarket";
  /// Application version.
  public static final version = "202212";
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
  /// Model indexes.
  public static final modelIxs = [
    "APRX" => 0,
    "ME" => 1,
    "ME2" => 2,
    "MM" => 3,
    "MX_MN" => 4,
    "QFIJO" => 5,
    "QMOV" => 6
  ];
}
