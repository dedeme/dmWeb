// Copyright 05-Aug-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// User inteface to construct propositions.
package pgs.wgs;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Tp;
import dm.It;
import data.Prop;
import data.AtomSet;
import data.OperatorSet;

class PropMaker {
  final wg: Domo;
  final accept: (PropT) -> Void;
  final cancel: () -> Void;

  final props: Array<PropT>;
  final pbuttons: Array<Domo> = [];
  final imgButtons: Array<Domo>;
  final replaceWg = Q("div");

  public function new (
    wg: Domo, accept: (PropT) -> Void, cancel: () -> Void
  ) {
    this.wg = wg;
    this.accept = accept;
    this.cancel = cancel;

    props = [Prop.mkAtomic(AtomSet.generator()())];
    imgButtons = [
      mkImgBt("revert", undo),
      mkImgBt("cancel", cancel),
      mkImgBt("ok", ok)
    ];

    show();
  }

  // View ----------------------------------------------------------------------

  function show (): Void {
    pbuttons.splice(0, pbuttons.length);
    final lastP = props[props.length - 1];
    final pspans = mkPropSpans(lastP, 0);
    switch (lastP) {
      case Atomic(p):
      case N(p):
      default: {
        pspans.e1.shift();
        pspans.e1.pop();
      }
    }
    imgButtons[0].disabled(Prop.eq(lastP, Prop.mkAtomic("p")));
    imgButtons[1].disabled(false);
    imgButtons[2].disabled(false);

    wg
      .removeAll()
      .add(Q("table")
        .att("align", "center")
        .add(Q("tr")
          .add(Q("td")
            .add(Q("div")
              .klass("frame")
                .add(Q("table")
                  .add(Q("tr")
                    .add(Q("td")
                      .style("text-align:left;")
                      .adds(pspans.e1))
                    .add(Q("td")
                      .style(
                          "width:5px;tex-align:right;"+
                          "border-left:1px solid black;"
                        ))
                    .add(Q("td").style("width:5px;").add(imgButtons[0]))
                    .add(Q("td").style("width:5px;").add(imgButtons[1]))
                    .add(Q("td").style("width:5px;").add(imgButtons[2])))
                  .add(Q("tr")
                    .add(Q("td")
                      .att("colspan", 5)
                      .add(Q("hr"))))
                  .add(Q("tr")
                    .add(Q("td")
                      .att("colspan", 5)
                      .add(replaceWg.removeAll())))
                )))))
    ;
  }

  function mkImgBt(img: String, action: () -> Void): Domo {
    return Q("button")
      .style("width:30px")
      .on(CLICK, action)
      .add(Q("img")
        .style("vertical-align:bottom")
        .att("src", "img/" + img + ".png"))
    ;
  }

  function mkPropSpans (prop: PropT, counter: Int): Tp<Array<Domo>, Int> {
    function par (left: Array<Domo>, symbol: String, right: Array<Domo>) {
      return [Q("span").text("(")]
        .concat(left)
        .concat([Q("span").html(symbol)])
        .concat(right)
        .concat([Q("span").text(")")])
      ;
    }

    return switch (prop) {
      case Atomic(p):
          final bt = Q("button")
            .style("font-family:monospace;cursor:pointer;width:30px")
            .klass("link")
            .on(CLICK, () -> replaceAction(prop, counter))
            .html(AtomSet.toString(p))
          ;
          pbuttons.push(bt);
          new Tp([bt], counter + 1);
      case N(p):
        final tp = mkPropSpans(p, counter);
        new Tp([Q("span").html(OperatorSet.N)].concat(tp.e1), tp.e2);
      case C(p, q):
        final tpP = mkPropSpans(p, counter);
        final tpQ = mkPropSpans(q, tpP.e2);
        new Tp(par(tpP.e1, OperatorSet.C, tpQ.e1), tpQ.e2);
      case A(p, q):
        final tpP = mkPropSpans(p, counter);
        final tpQ = mkPropSpans(q, tpP.e2);
        new Tp(par(tpP.e1, OperatorSet.A, tpQ.e1), tpQ.e2);
      case K(p, q):
        final tpP = mkPropSpans(p, counter);
        final tpQ = mkPropSpans(q, tpP.e2);
        new Tp(par(tpP.e1, OperatorSet.K, tpQ.e1), tpQ.e2);
      case E(p, q):
        final tpP = mkPropSpans(p, counter);
        final tpQ = mkPropSpans(q, tpP.e2);
        new Tp(par(tpP.e1, OperatorSet.E, tpQ.e1), tpQ.e2);
      case D(p, q):
        final tpP = mkPropSpans(p, counter);
        final tpQ = mkPropSpans(q, tpP.e2);
        new Tp(par(tpP.e1, OperatorSet.D, tpQ.e1), tpQ.e2);
    }
  }

  function propSelector (propId: String, ix: Int): Void {
    function hseparator () {
      return Q("span").html("&nbsp;");
    }
    function vseparator () {
      return Q("div").style("padding-top:4px");
    }
    function mkBt (prop: PropT): Domo {
      return Q("button")
        .style("width:60px")
        .on(CLICK, () -> replaceAndPush(ix, prop))
        .html(Prop.toString(prop))
      ;
    }
    function mkAtomics (id: String): Array<Domo> {
      return It.range(10).reduce(
        [mkBt(Atomic(id))],
        (r, i) -> {
          r.push(hseparator());
          r.push(mkBt(Atomic(id + i)));
          return r;
        }
      );
    }
    replaceWg
      .removeAll()
      .add(Q("div")
        .adds([
            mkBt(Prop.mkN(Prop.mkAtomic("p"))),
            hseparator(),
            mkBt(Prop.mkC(Prop.mkAtomic("p"), Prop.mkAtomic("q"))),
            hseparator(),
            mkBt(Prop.mkA(Prop.mkAtomic("p"), Prop.mkAtomic("q"))),
            hseparator(),
            mkBt(Prop.mkK(Prop.mkAtomic("p"), Prop.mkAtomic("q"))),
            hseparator(),
            mkBt(Prop.mkE(Prop.mkAtomic("p"), Prop.mkAtomic("q"))),
            hseparator(),
            mkBt(Prop.mkD(Prop.mkAtomic("p"), Prop.mkAtomic("q")))
          ]))
      .add(vseparator())
      .add(Q("div").adds(mkAtomics("p")))
      .add(vseparator())
      .add(Q("div").adds(mkAtomics("q")))
      .add(vseparator())
      .add(Q("div").adds(mkAtomics("r")))
      .add(vseparator())
      .add(Q("div").add(mkImgBt("cancel", show)))
    ;
  }

  // Control -------------------------------------------------------------------

  function replace (p: PropT, ix: Int, rp: PropT): Tp<PropT, Int> {
    return switch (p) {
      case Atomic(q): new Tp(ix == 0 ? rp : p, ix - 1);
      case N(q):
        final tp = replace(q, ix, rp);
        new Tp(Prop.mkN(tp.e1), tp.e2);
      case C(q, r):
        final tpQ = replace(q, ix, rp);
        final tpR = replace(r, tpQ.e2, rp);
        new Tp(Prop.mkC(tpQ.e1, tpR.e1), tpR.e2);
      case A(q, r):
        final tpQ = replace(q, ix, rp);
        final tpR = replace(r, tpQ.e2, rp);
        new Tp(Prop.mkA(tpQ.e1, tpR.e1), tpR.e2);
      case K(q, r):
        final tpQ = replace(q, ix, rp);
        final tpR = replace(r, tpQ.e2, rp);
        new Tp(Prop.mkK(tpQ.e1, tpR.e1), tpR.e2);
      case E(q, r):
        final tpQ = replace(q, ix, rp);
        final tpR = replace(r, tpQ.e2, rp);
        new Tp(Prop.mkE(tpQ.e1, tpR.e1), tpR.e2);
      case D(q, r):
        final tpQ = replace(q, ix, rp);
        final tpR = replace(r, tpQ.e2, rp);
        new Tp(Prop.mkD(tpQ.e1, tpR.e1), tpR.e2);
    }
  }

  function replaceAction (prop: PropT, ix: Int) {
    It.from(imgButtons).each(e -> e.disabled(true));
    It.from(pbuttons).eachIx((b, i) -> {
      b.disabled(true);
      if (i == ix) b.klass("frame");
    });
    switch (prop) {
      case Atomic(p): propSelector(p, ix);
      default: throw new haxe.Exception("'prop' must be atomic");
    }
  }

  function replaceAndPush (ix: Int, rp: PropT) {
    final prIx = replace(props[props.length - 1], ix, rp);
    props.push(prIx.e1);
    show();
  }

  function undo (): Void {
    props.pop();
    show();
  }

  function ok (): Void {
    accept(props[props.length - 1]);
  }

}
