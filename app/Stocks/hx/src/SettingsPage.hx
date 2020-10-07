// Copyright 22-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Store;
import I18n._;
import I18n._args;
import Cts;

/// Settings page.
class SettingsPage {
  // Control -------------------------------------------------------------------

  static function changeLang (): Void {
    final key = Cts.langKey;
    switch (Store.get(key)) {
      case Some(l):
        if (l == "es") Store.put(key, "en");
        else Store.put(key, "es");
      case None: Store.put(key, "en");
    }
    js.Browser.location.assign("");
  }

  static function changePass (wg: Domo): Void {
    ChangePass.mk(wg, Cts.app, () -> mk(wg));
  }

  /// View ---------------------------------------------------------------------

  /// Constructor.
  public static function mk (wg: Domo): Void {
    final lang = switch (Store.get(Cts.langKey)) {
      case Some(l): l;
      case None: "es";
    }

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
                    [lang == "es" ? "EN" : "ES"]
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
