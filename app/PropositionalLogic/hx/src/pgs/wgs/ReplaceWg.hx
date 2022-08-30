// Copyright 12-Aug-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs.wgs;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.It;
import data.AtomSet;
import data.Prop;
import data.Replacement;

class ReplaceWg {
  final wg: Domo;
  final th: PropT;
  final cancel: () -> Void;
  final accept: (Array<Replacement>) -> Void;
  final replacements: Array<Replacement>;

  final propMakerDiv = Q("div");
  final acceptBt: Domo;
  final cancelBt: Domo;
  final idBts: Array<Domo> = [];

  public function new (
    wg: Domo, th: PropT,
    cancel: () -> Void, accept: (Array<Replacement>) -> Void
  ) {
    this.wg = wg;
    this.th = th;
    this.cancel = cancel;
    this.accept = accept;
    replacements = Prop.atomicIds(th)
      .map(id -> new Replacement(id, Prop.mkAtomic(id)))
    ;
    replacements.sort((r1, r2) -> dm.Str.compare(r1.key, r2.key));

    acceptBt = Q("button")
      .style("width:50px")
      .add(Q("img")
        .style("vertical-align:bottom")
        .att("src", "img/ok.png"))
      .on(CLICK, () -> accept(replacements))
    ;
    cancelBt = Q("button")
      .style("width:50px")
      .add(Q("img")
        .style("vertical-align:bottom")
        .att("src", "img/cancel.png"))
      .on(CLICK, cancel)
    ;

    show();
  }

  // View ----------------------------------------------------------------------

  function show (): Void {
    idBts.splice(0, idBts.length);
    function mkTr (r: Replacement): Domo {
      final ix = idBts.length;
      final bt = Q("button")
        .style("width:50px")
        .text(AtomSet.toString(r.key))
        .on(CLICK, () -> replace(ix))
      ;
      idBts.push(bt);

      return Q("tr")
        .add(Q("td")
          .style("width:5px;")
          .add(bt))
        .add(Q("td")
          .style("text-align:left;")
          .html(Prop.toString(r.value)))
      ;
    }

    acceptBt.disabled(false);
    cancelBt.disabled(false);
    wg
      .removeAll()
      .add(Q("hr"))
      .add(Q("table")
        .att("align", "center")
        .style("border-collapse : collapse;")
        .klass("border")
        .add(Q("tr")
          .add(Q("td")
            .att("colspan", 2)
            .add(Q("table")
              .att("align", "center")
              .add(Q("tr")
                .add(Q("td")
                  .add(acceptBt))
                .add(Q("td")
                  .add(cancelBt))))))
        .add(Q("tr")
          .add(Q("td")
            .klass("border")
            .att("colspan", 2)
            .html(Prop.toString(th))))
        .add(Q("tr")
          .add(Q("td")
            .klass("border")
            .att("colspan", 2)
            .html(Prop.toString(Prop.replace(th, replacements)))))
        .adds(replacements.map(mkTr)))
      .add(propMakerDiv
        .removeAll())
    ;
  }

  // Control -------------------------------------------------------------------

  function replace (ix: Int): Void {
    It.from(idBts).eachIx((b, i) -> {
      b.disabled(true);
      if (i == ix) b.klass("frame");
    });
    acceptBt.disabled(true);
    cancelBt.disabled(true);

    final r = replacements[ix];
    new PropMaker(
      propMakerDiv,
      prop -> {
        replacements[ix] = new Replacement(r.key, prop);
        show();
      },
      show
    );
  }
}
