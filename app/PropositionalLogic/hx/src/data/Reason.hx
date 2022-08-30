// Copyright 02-Aug-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

using StringTools;
import haxe.Exception as Exc;
import dm.Js;
import dm.It;
import data.Prop;

/// Types of reason.
enum ReasonT {
  Sup(p: PropT);
  Theorem(id: String, index: Int, replacements: Array<Replacement>);
  IN;
  EN;
  IC;
  EC;
  IA;
  EA;
  IK;
  EK;
  IE;
  EE;
  ID;
  ED;
}

class Reason {

  /// Supposition.
  public static function mkSup (p: PropT): ReasonT {
    if (!Prop.isValid(p)) throw new Exc("'" + p + "' is not a valid proposition");
    return Sup(p);
  }

  /// Theorem.
  public static function mkTheorem(
    id: String, index: Int, replacements: Array<Replacement>
  ): ReasonT {
    return Theorem(id, index, replacements);
  }

  /// Introduction of negation
  /// NOTE: ix1 and ix2 are interchangeable.
  public static function mkIN (): ReasonT {
    return IN;
  }

  /// Elimination of negation.
  public static function mkEN (): ReasonT {
    return EN;
  }

  /// Introduction of implication.
  public static function mkIC (): ReasonT {
    return IC;
  }

  /// Elimination of implication.
  public static function mkEC (): ReasonT {
    return EC;
  }

  /// Introduction of disjuction.
  public static function mkIA (): ReasonT {
    return IA;
  }

  /// Elimination of disjuction.
  public static function mkEA (): ReasonT {
    return EA;
  }

  /// Introduction of conjuction.
  public static function mkIK (): ReasonT {
    return IK;
  }

  /// Elimination of conjuction.
  public static function mkEK (): ReasonT {
    return EK;
  }

  /// Introduction of equivalence.
  public static function mkIE (): ReasonT {
    return IE;
  }

  /// Elimination of equivalence.
  public static function mkEE (): ReasonT {
    return EE;
  }

  /// Introduction of exclusion.
  public static function mkID (): ReasonT {
    return ID;
  }

  /// Elimination of exclusion.
  public static function mkED (): ReasonT {
    return ED;
  }

  public static function toJs (r: ReasonT): Js {
    return Js.wa(
      switch(r) {
        case Sup(p): [Js.ws("sup"), Prop.toJs(p)];
        case Theorem(id, index, rpls): [
          Js.ws("the"),
          Js.ws(id),
          Js.wi(index),
          Js.wa(rpls.map(e -> e.toJs()))
        ];
        case IN: [Js.ws("IN")];
        case EN: [Js.ws("EN")];
        case IC: [Js.ws("IC")];
        case EC: [Js.ws("EC")];
        case IA: [Js.ws("IA")];
        case EA: [Js.ws("EA")];
        case IK: [Js.ws("IK")];
        case EK: [Js.ws("EK")];
        case IE: [Js.ws("IE")];
        case EE: [Js.ws("EE")];
        case ID: [Js.ws("ID")];
        case ED: [Js.ws("ED")];
      }
    );
  }

  public static function fromJs (js: Js): ReasonT {
    final a = js.ra();
    return switch(a[0].rs()) {
      case "sup": mkSup(Prop.fromJs(a[1]));
      case "the": mkTheorem(
        a[1].rs(),
        a[2].ri(),
        a[3].ra().map(Replacement.fromJs)
      );
      case "IN": mkIN();
      case "EN": mkEN();
      case "IC": mkIC();
      case "EC": mkEC();
      case "IA": mkIA();
      case "EA": mkEA();
      case "IK": mkIK();
      case "EK": mkEK();
      case "IE": mkIE();
      case "EE": mkEE();
      case "ID": mkID();
      case "ED": mkED();
      default: throw new Exc("'" + js + "' is not a valid serialized reason");
    }
  }

  public static function level (reasons: Array<ReasonT>): Int {
    return It.from(reasons).reduce(0, (r, e) -> switch (e) {
      case Sup(p): r + 1;
      case IC: r - 1;
      case IN: r - 1;
      default: r;
    });
  }
}
