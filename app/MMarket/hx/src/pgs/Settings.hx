// Copyright 15-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.Store;
import I18n._;
import I18n._args;

/// Settings page.
class Settings {
  final wg: Domo;
  final lang: String;

  public function new (wg: Domo, lang: String) {
    this.wg = wg;
    this.lang = lang;
  }

  // View ----------------------------------------------------------------------

  public function show (): Void {
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
                .add(Ui.link(e -> changePass())
                  .klass("link")
                  .html(_("Change Password"))))))))
    ;
  }

  // Control -------------------------------------------------------------------

  function changeLang (): Void {
    Store.put(Cts.langKey, lang == "es" ? "en": "es");
    js.Browser.location.assign("");
  }

  function changePass (): Void {
    new ChangePass(wg, () -> show()).show();
  }

}
