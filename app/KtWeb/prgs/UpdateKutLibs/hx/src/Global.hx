// Copyright 03-Nov-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import dm.Client;
import dm.Ui.Q;
import I18n._;

/// Global variables for every program.
class Global {
  static final KwWebAppName = "KtWeb";

  /// Application client.
  public static final client = new Client(true, KwWebAppName, () -> {
    final wg = Q("div");
    new MsgPg(wg, _("Session is expired.")).show();
    Q("@body")
      .removeAll()
      .add(wg)
      .add(Cts.foot)
    ;
  });
}
