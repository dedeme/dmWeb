// Copyright 31-Jul-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.Store;
import data.AtomSet;
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
    final set = AtomSet.currentSet();
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
                .add(Q("span")
                  .html(_("Select Leters for Atomic Propositions")))
                .add(Q("br"))
                .add(Q("table")
                  .att("align", "center")
                  .klass("flat")
                  .add(Q("tr")
                    .add(Q("td")
                      .add(Q("input")
                        .att("type", "radio")
                        .att("name", "set")
                        .checked(set == 0)
                        .on(CLICK, () -> setAtomicSet(0))))
                    .add(Q("td")
                      .html(AtomSet.sample(0).join(", "))))
                  .add(Q("tr")
                    .add(Q("td")
                      .add(Q("input")
                        .att("type", "radio")
                        .att("name", "set")
                        .checked(set == 1)
                        .on(CLICK, () -> setAtomicSet(1))))
                    .add(Q("td")
                      .html(AtomSet.sample(1).join(", "))))
                  .add(Q("tr")
                    .add(Q("td")
                      .add(Q("input")
                        .att("type", "radio")
                        .att("name", "set")
                        .checked(set == 2)
                        .on(CLICK, () -> setAtomicSet(2))))
                    .add(Q("td")
                      .html(AtomSet.sample(2).join(", "))))))
              .add(Q("p")
                .html("<p></p>"))
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

  function setAtomicSet(set: Int): Void {
    Store.put(Cts.atomSetIx, ""+set);
  }

  function changeLang (): Void {
    Store.put(Cts.langKey, lang == "es" ? "en": "es");
    js.Browser.location.assign("");
  }

  function changePass (): Void {
    new ChangePass(wg, () -> show()).show();
  }

}
