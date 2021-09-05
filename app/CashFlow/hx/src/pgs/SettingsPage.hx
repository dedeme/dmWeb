// Copyright 27-Aug-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import I18n._;
import I18n._args;

/// Settings page.
class SettingsPage {
  // Control -------------------------------------------------------------------

  static function changeLang (): Void {
    if (Storage.getLang() == "es") Storage.setLang("en");
    else Storage.setLang("es");
    js.Browser.location.reload(true);
  }

  static function changePass (wg: Domo): Void {
    ChangePass.mk(wg, Cts.app, () -> mk(wg));
  }

  /// View ---------------------------------------------------------------------

  /// Constructor.
  public static function mk (wg: Domo): Void {
    wg
      .removeAll()
      .add(Q("div")
        .style("text-align:center")
        .add(Q("div")
          .klass("head")
          .html(_("Settings")))
        .add(Q("table")
          .att("align", "center")
          .add(Q("tr")
            .add(Q("td")
              .klass("frame")
              .add(Q("div")
                .add(Ui.link(e -> changeLang())
                  .klass("link")
                  .html(_args(
                    _("Change Language to %0"),
                    [Storage.getLang() == "es" ? "EN" : "ES"]
                  ))))
              .add(Q("p")
                .html("<p></p>"))
              .add(Q("div")
                .add(Ui.link(e -> changePass(wg))
                  .klass("link")
                  .html(_("Change Password"))))))))
    ;
  }
}
