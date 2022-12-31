// Copyright 31-Dic-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package wgs;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.It;
import data.Dim;
import I18n._;

class DimSelector {
  final wg: Domo;
  var dim: String;
  final cancel: Void -> Void;
  final accept: Dim -> Void;

  public function new (
    wg: Domo, dim: String,
    cancel: Void -> Void, accept: Dim -> Void
  ) {
    this.wg = wg;
    this.dim = dim;
    this.cancel = cancel;
    this.accept = accept;
  }

  // View ----------------------------------------------------------------------
  public function show (): Void {
    wg
      .removeAll()
      .add(Q("table")
        .att("align", "center")
        .adds(It.fromMap(Cts.dims).map(kv ->
          Q("tr")
            .add(Q("td")
              .add(Q("input")
                .att("type", "radio")
                .att("name", "dims")
                .checked(dim == kv.e1)
                .on(CLICK, () -> dim = kv.e1)))
            .add(Q("td")
              .style("text-align: left")
              .text(kv.e1))
          )))
      .add(Q("hr"))
      .add(Q("table")
        .att("align", "center")
        .add(Q("tr")
          .add(Q("td")
            .add(Q("button")
              .text(_("Cancel"))
              .on(CLICK, cancel))
            .add(Q("span").html("&nbsp;&nbsp;"))
            .add(Q("button")
              .text(_("Accept"))
              .on(CLICK, () -> accept(Cts.dims[dim]))))))
    ;
  }
}
