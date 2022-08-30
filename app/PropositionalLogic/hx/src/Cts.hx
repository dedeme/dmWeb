// Copyright 31-Jul-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import dm.Client;
import dm.Ui.Q;
import dm.ModalBox;
import I18n._;

/// Constants.
class Cts {
  /// Application name.
  public static final appName = "PropositionalLogic";
  /// Application version.
  public static var version(default, null) = "202207";
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
  /// Modal wait.
  public static final wait = new ModalBox (
    dm.Ui.img("wait.gif")
    , false
  );
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
  /// Store lang key.
  public static final langKey = Cts.appName + "_lang";
  /// Store atomic propositions set key.
  public static final atomSetIx = Cts.appName + "_set";
}
