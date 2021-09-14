// Copyright 27-Aug-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import I18n._;

/// Year page.
class Year {
  final wg: Domo;
  final years: Array<String>;
  final selected: String;
  function new (wg: Domo, years: Array<String>, selected: String) {
    years.sort(dm.Str.compare);

    this.wg = wg;
    this.years = years;
    this.selected = selected;

    view();
  }

  // View ----------------------------------------------------------------------

  function view (): Void {
    function td(y) {
      return Q("td")
        .style("text-align:center")
        .add(
          y == selected
            ? Q("span")
              .klass("frame")
              .html("·" + y + "·")
            : Ui.link(ev -> changeYear(y))
                .klass("link")
                .html("·" + y + "·"))
      ;
    }

    wg
      .removeAll()
      .add(Q("div")
        .klass("head")
        .html(_("Change of Year")))
      .add(Q("table")
        .att("align", "center")
        .adds(years.map(y ->
            Q("tr")
              .add(td(y))
          )))
    ;
  }

  // Control -------------------------------------------------------------------

  function changeYear (year: String): Void {
    js.Browser.location.replace("?year&" + year);
  }

  // Static --------------------------------------------------------------------

  /// Constructor.
  ///   wg          : Widget.
  ///   selectedYear: Selected year.
  public static function mk (wg: Domo, selectedYear: String): Void {
    Cts.client.send([
      "source" => Js.ws("Year"),
      "rq" => Js.ws("idata")
    ], rp -> {
      final years = rp["years"].ra().map(e -> e.rs());
      new Year(wg, years, selectedYear);
    });
  }
}
