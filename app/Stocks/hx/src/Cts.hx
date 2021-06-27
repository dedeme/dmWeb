// Copyright 25-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

using StringTools;

import dm.Client;
import dm.Domo;
import dm.Ui.Q;
import dm.Dec;
import I18n._;
import dm.Opt;

/// Constants and global functions.
class Cts {
  /// Application name.
  public static final app = "Stocks";
  /// Application version.
  public static final version = "202009";
  /// Store language key.
  public static final langKey = "Lang_" + app;
  /// Initial year.
  public static final initialYear = "2020";
  /// Number of investors (managers)
  public static final investors = 3;
  /// Page foot.
  public static final foot = Q("table")
    .klass("main")
    .add(Q("tr")
      .add(Q("td")
        .add(Q("hr"))))
    .add(Q("tr")
      .add(Q("td")
        .style("text-align: right;color:#808080;font-size:x-small;")
        .html('- © ºDeme. ${app} (${version}) -')))
  ;
  /// Application client.
  public static final client = new Client(true, app, () -> {
    final wg = Q("div");
    MsgPage.mk(wg, app, _("Session is expired."));
    Q("@body")
      .removeAll()
      .add(wg)
      .add(foot)
    ;
  });
  /// Returns the float value of 'wg' with 'dec' decimals.
  public static function float (wg: Domo, dec: Int): Option<Float> {
    final n = cast(wg.getValue(), String)
      .trim().replace(".", "").replace(",", ".")
    ;
    if (n == "") return Some(0.0);
    final r = Std.parseFloat(n);
    return Math.isNaN(r) ? None : Some(Dec.round(r, dec));
  }
  /// Returns the int value of 'wg'
  public static function int (wg: Domo): Option<Int> {
    final n = cast(wg.getValue(), String).trim().replace(".", "");
    if (n == "") return Some(0);
    final r = Std.parseInt(n);
    return r == null ? None : Some(r);
  }
}
