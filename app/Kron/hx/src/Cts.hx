// Copyright 12-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

using StringTools;

import dm.Client;
import dm.Ui.Q;
import dm.Dec;
import pgs.MsgPg;
import I18n._;
import dm.Opt;

/// Constants and global functions.
class Cts {
  /// Application name.
  public static final appName = "Kron";
  /// Application version.
  public static final version = "202112";
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
}
