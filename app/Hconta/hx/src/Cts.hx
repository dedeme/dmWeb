// Copyright 25-Jun-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

using StringTools;

import dm.Client;
import dm.Ui.Q;
import I18n._;
import haxe.ds.Option;

/// Constants and global functions.
class Cts {
  /// Application name.
  public static final app = "Hconta";
  /// Application version.
  public static final version = "202006";
  /// Cash count
  public static final cash = "57200";
  /// Capital count
  public static final capital = "10200";
  /// Capital count
  public static final results = "12000";
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
  /// Maximun table length.
  public static final tableLen = 20;
  /// Number of entries for most used accounts.
  public static final mostUsedLen = 20;
  /// Length of left help entries.
  public static final helpLen = 35;
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
  /// Returns and formated account.
  public static function accFormat (acc: String): String {
    return acc.length > 3 ? acc.substring(0, 3) + "." + acc.substring(3) : acc;
  }
  /// Returns the float value of 'n'
  public static function float (n: String): Option<Float> {
    n = n.trim().replace(".", "").replace(",", ".");
    if (n == "") return Some(0.0);
    final r = Std.parseFloat(n);
    return Math.isNaN(r) ? None : Some(r);
  }
}
