// Copyright 20-Jan-2023 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import dm.Client;
import dm.Ui.Q;
import I18n._;

/// Constants.
class Cts {
  /// Application name.
  public static final appName = "KtWeb:JstbEditor";
  /// Application version.
  public static var version(default, null) = "202301";
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
}
