// Copyright 16-Oct-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs.settings.settings;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import I18n._;
import I18n._args;

/// Settings - settings page.
class Settings {
  final wg: Domo;
  final lang: String;

  /// Constructor
  ///   wg    : Container.
  ///   lang  : Language.
  public function new (wg: Domo, lang: String) {
    this.wg = wg;
    this.lang = lang;

    view();
  }

  // view ----------------------------------------------------------------------

  function view(): Void {
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
                .add(Ui.link(e -> changeLang(lang))
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

  // control -------------------------------------------------------------------

  function changeLang (lang: String) {
    Cts.client.send([
      "module" => Js.ws("settings"),
      "source" => Js.ws("settings"),
      "rq" => Js.ws("setLang"),
      "lang" => Js.ws(lang == "en" ? "es" : "en")
    ], rp -> {
      js.Browser.location.reload();
    });
  }

  function changePass (wg: Domo): Void {
    new ChangePass(wg, () -> view());
  }
}
