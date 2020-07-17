// Copyright 12-Jul-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pages;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.Dec;
import I18n._;

/// News page.
class Weights {
  /// Constructor.
  public static function mk (wg: Domo): Void {
    function fmt (n: Float): String {
      return Dec.toIso(n * 100, 2) + "%";
    }

    Cts.client.send([
      "source" => Js.ws("Weights"),
      "rq" => Js.ws("read")
    ], rp -> {
      final source = rp["source"].rf();
      final author = rp["author"].rf();
      final words = rp["words"].rf();
      wg
        .removeAll()
        .add(Q("div")
          .style("text-align:center")
          .add(Q("div")
            .klass("head")
            .html(_("Weights")))
          .add(Q("table")
            .att("align", "center")
            .att("cellspacing", 0)
            .add(Q("tr")
              .add(Q("td")
                .klass("desc")
                .text(_("Source") + ": "))
              .add(Q("td")
                .klass("prer")
                .text(fmt(source))))
            .add(Q("tr")
              .add(Q("td")
                .att("colspan", "2")))
            .add(Q("tr")
              .add(Q("td")
                .klass("desc")
                .text(_("Author") + ": "))
              .add(Q("td")
                .klass("prer")
                .text(fmt(author))))
            .add(Q("tr")
              .add(Q("td")
                .att("colspan", "2")))
            .add(Q("tr")
              .add(Q("td")
                .klass("desc")
                .text(_("Words") + ": "))
              .add(Q("td")
                .klass("prer")
                .text(fmt(words))))))
      ;
    });
  }
}
