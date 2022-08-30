// Copyright 02-Aug-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import haxe.Exception as Exc;
import dm.Js;
import dm.Tp;
import dm.It;
import data.Prop;
import data.Reason;
import data.AtomSet;

/// Demonstration.
class Demo {
  public final reasons: Array<ReasonT>;
  public final conclusion: PropT;

  /// Constructor
  ///   reasons: Reasons to make a Demo. They will be regularized.
  ///   conclusion: Conclusion of reasons. It will be regularized.
  public function new (reasons: Array<ReasonT>, conclusion: PropT) {
    this.reasons = reasons;
    this.conclusion = conclusion;
  }

  /// Returns the Demo name.
  public function name (): String {
    function name2 (prop: PropT): String {
      return switch (prop) {
        case Atomic(id): "";
        case N(p): "N" + name2(p);
        case C(p, q): "C" + name2(p) + name2(q);
        case A(p, q): "A" + name2(p) + name2(q);
        case K(p, q): "K" + name2(p) + name2(q);
        case E(p, q): "E" + name2(p) + name2(q);
        case D(p, q): "D" + name2(p) + name2(q);
      }
    }
    return name2(conclusion);
  }

  /// Returns a regularized demo.
  public function regularize (): Demo {
    final gen = AtomSet.generator();
    final dic: Array<Tp<String, String>> = [];
    // If there is no traslation, returns "".
    function translation (id: String): String {
      switch (It.from(dic).find(tp -> tp.e1 == id)) {
        case Some(tp): return tp.e2;
        case None: return "";
      }
    }
    function pregularize (prop: PropT): PropT {
      return switch (prop) {
        case Atomic(id): switch (translation(id)) {
          case "":
            final newId = gen();
            dic.push(new Tp(id, newId));
            Atomic(newId);
          case newId:
            Atomic(newId);
        };
        case N(p): Prop.mkN(pregularize(p));
        case C(p, q): Prop.mkC(pregularize(p), pregularize(q));
        case A(p, q): Prop.mkA(pregularize(p), pregularize(q));
        case K(p, q): Prop.mkK(pregularize(p), pregularize(q));
        case E(p, q): Prop.mkE(pregularize(p), pregularize(q));
        case D(p, q): Prop.mkD(pregularize(p), pregularize(q));
      }
    }

    final newConclusion = pregularize(conclusion);
    final newReasons = reasons.map(r -> switch (r) {
      case Sup(p): Reason.mkSup(pregularize(p));
      case Theorem(id, isPremise, rpls): Reason.mkTheorem(
        id,
        isPremise,
        rpls.map(r -> new Replacement(r.key, pregularize(r.value)))
      );
      default: r;
    });

    return new Demo(newReasons, newConclusion);
  }

  /// Returns true if the demonstration depends on theorem 'id'.
  ///   id: Theorem identifier in form 'name'-'number'.
  public function dependsOnTheorem (id: String): Bool {
    for (r in reasons) {
      switch (r) {
        case Theorem(thId, _, _) if (id == thId): return true;
        default:
      }
    }
    return false;
  }

  public function toJs (): Js {
    return Js.wa([
      Js.wa(reasons.map(Reason.toJs)),
      Prop.toJs(conclusion)
    ]);
  }

  public static function fromJs (js: Js): Demo {
    final a = js.ra();
    return new Demo(
      a[0].ra().map(Reason.fromJs),
      Prop.fromJs(a[1])
    );
  }
}
