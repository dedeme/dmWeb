// Copyright 18-Jun-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import dm.Client;
import dm.Ui.Q;
import I18n._;

/// Constants.
class Cts {
  /// Application version.
  public static var version(default, null) = "202111";
  /// Application client.
  public static var client = new Client(true, cm.Cts.app, () -> {
    pgs.MsgPage.mk(Q("@body"), cm.Cts.app, _("Session is expired."));
  });
}
