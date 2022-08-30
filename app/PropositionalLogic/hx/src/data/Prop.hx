// Copyright 02-Aug-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

using StringTools;
import haxe.Exception as Exc;
import dm.Dec;
import dm.Js;
import dm.It;
import dm.Opt;

/// Types of proposition.
enum PropT {
  Atomic(id: String);
  N(pr: PropT);
  C(pr1: PropT, pr2: PropT);
  A(pr1: PropT, pr2: PropT);
  K(pr1: PropT, pr2: PropT);
  E(pr1: PropT, pr2: PropT);
  D(pr1: PropT, pr2: PropT);
}

class Prop {
  /// Returns an atomic proposition.
  public static function mkAtomic (id: String): PropT {
    if (id == "") throw new Exc("Empty identifier");
    if (id.length > 2)
      throw new Exc("Identifier '"+ id + "' with more than 2 characters");
    if (!"pqr".contains(id.charAt(0)))
      throw new Exc("Identifier '" + id + "' does not start with 'p', 'q' nor 'r'");
    if (id.length == 2 && !Dec.digits(id.charAt(1)))
      throw new Exc("Second character of identifier '" + id + "' is not a digit");

    return Atomic(id);
  }

  /// Returns a negation.
  public static function mkN (p: PropT): PropT {
    if (!isValid(p)) throw new Exc("'" + p + "' is not a valid proposition");
    return N(p);
  }

  /// Returns an implication.
  public static function mkC (p1: PropT, p2: PropT): PropT {
    if (!isValid(p1)) throw new Exc("'" + p1 + "' is not a valid proposition");
    if (!isValid(p2)) throw new Exc("'" + p2 + "' is not a valid proposition");
    return C(p1, p2);
  }

  /// Returns a disjunction.
  public static function mkA (p1: PropT, p2: PropT): PropT {
    if (!isValid(p1)) throw new Exc("'" + p1 + "' is not a valid proposition");
    if (!isValid(p2)) throw new Exc("'" + p2 + "' is not a valid proposition");
    return A(p1, p2);
  }

  /// Returns a conjunction.
  public static function mkK (p1: PropT, p2: PropT): PropT {
    if (!isValid(p1)) throw new Exc("'" + p1 + "' is not a valid proposition");
    if (!isValid(p2)) throw new Exc("'" + p2 + "' is not a valid proposition");
    return K(p1, p2);
  }

  /// Returns an equivalence.
  public static function mkE (p1: PropT, p2: PropT): PropT {
    if (!isValid(p1)) throw new Exc("'" + p1 + "' is not a valid proposition");
    if (!isValid(p2)) throw new Exc("'" + p2 + "' is not a valid proposition");
    return E(p1, p2);
  }

  /// Returns an exclusion.
  public static function mkD (p1: PropT, p2: PropT): PropT {
    if (!isValid(p1)) throw new Exc("'" + p1 + "' is not a valid proposition");
    if (!isValid(p2)) throw new Exc("'" + p2 + "' is not a valid proposition");
    return D(p1, p2);
  }

  /// Returns 'true' if 'p' is a valid proposition.
  public static function isValid (p: PropT): Bool {
    switch (p) {
      case Atomic(id): {
        if (id == "" || id.length > 2) return false;
        if (!"pqr".contains(id.charAt(0))) return false;
        if (id.length == 2 && !Dec.digits(id.charAt(1))) return false;
        return true;
      }
      case N(p): return isValid(p);
      case C(p1, p2): return isValid(p1) && isValid(p2);
      case A(p1, p2): return isValid(p1) && isValid(p2);
      case K(p1, p2): return isValid(p1) && isValid(p2);
      case E(p1, p2): return isValid(p1) && isValid(p2);
      case D(p1, p2): return isValid(p1) && isValid(p2);
    }
  }

  /// Returns 'true' if 'p1' is equals to 'p2'
  public static function eq (p1: PropT, p2: PropT): Bool {
    return switch (p1) {
      case Atomic(id1): switch (p2) {
        case Atomic(id2): id1 == id2;
        default: false;
      }
      case N(q1): switch (p2) {
        case N(q2): eq(q1, q2);
        default: false;
      }
      case C(q1, r1): switch (p2) {
        case C(q2, r2): eq(q1, q2) && eq(r1, r2);
        default: false;
      }
      case A(q1, r1): switch (p2) {
        case A(q2, r2): eq(q1, q2) && eq(r1, r2);
        default: false;
      }
      case K(q1, r1): switch (p2) {
        case K(q2, r2): eq(q1, q2) && eq(r1, r2);
        default: false;
      }
      case E(q1, r1): switch (p2) {
        case E(q2, r2): eq(q1, q2) && eq(r1, r2);
        default: false;
      }
      case D(q1, r1): switch (p2) {
        case D(q2, r2): eq(q1, q2) && eq(r1, r2);
        default: false;
      }
    }
  }

  /// Returns a new proposition replacing atomics occurrences in 'p' using
  /// 'replacemients'.
  public static function replace (
    p: PropT, replacements: Array<Replacement>
  ): PropT {
    return switch(p) {
      case Atomic(id): switch(It.from(replacements).find(e -> e.key == id)) {
        case Some(rpl): rpl.value;
        case None: p;
      }
      case N(q): N(replace(q, replacements));
      case C(p1, p2): C(replace(p1, replacements), replace(p2, replacements));
      case A(p1, p2): A(replace(p1, replacements), replace(p2, replacements));
      case K(p1, p2): K(replace(p1, replacements), replace(p2, replacements));
      case E(p1, p2): E(replace(p1, replacements), replace(p2, replacements));
      case D(p1, p2): D(replace(p1, replacements), replace(p2, replacements));
    }
  }

  /// Returns identifiers of atomic propositions of 'prop'
  public static function atomicIds (prop: PropT): Array<String> {
    function flat (ss: Array<String>): Array<String> {
      final r: Array<String> = [];
      for (s in ss) {
        if (!r.contains(s)) r.push(s);
      }
      return r;
    }

    return switch (prop) {
      case Atomic(id): [id];
      case N(p): atomicIds(p);
      case C(p, q): flat(atomicIds(p).concat(atomicIds(q)));
      case A(p, q): flat(atomicIds(p).concat(atomicIds(q)));
      case K(p, q): flat(atomicIds(p).concat(atomicIds(q)));
      case E(p, q): flat(atomicIds(p).concat(atomicIds(q)));
      case D(p, q): flat(atomicIds(p).concat(atomicIds(q)));
    }
  }

  public static function toString (prop: PropT): String {
    function aux (prop): String {
      return switch (prop) {
        case Atomic(id): AtomSet.toString(id);
        case N(p): OperatorSet.N + aux(p);
        case C(p1, p2): "(" + aux(p1) + OperatorSet.C + aux(p2) + ")";
        case A(p1, p2): "(" + aux(p1) + OperatorSet.A + aux(p2) + ")";
        case K(p1, p2): "(" + aux(p1) + OperatorSet.K + aux(p2) + ")";
        case E(p1, p2): "(" + aux(p1) + OperatorSet.E + aux(p2) + ")";
        case D(p1, p2): "(" + aux(p1) + OperatorSet.D + aux(p2) + ")";
      }
    }
    var r = aux(prop);
    if (r.charAt(0) == "(") r = r.substring(1, r.length - 1);
    return r;
  }

  public static function toJs (p: PropT): Js {
    return Js.wa(
      switch(p) {
        case Atomic(id): [Js.ws("0"), Js.ws(id)];
        case N(q): [Js.ws("N"), toJs(q)];
        case C(p1, p2): [Js.ws("C"), toJs(p1), toJs(p2)];
        case A(p1, p2): [Js.ws("A"), toJs(p1), toJs(p2)];
        case K(p1, p2): [Js.ws("K"), toJs(p1), toJs(p2)];
        case E(p1, p2): [Js.ws("E"), toJs(p1), toJs(p2)];
        case D(p1, p2): [Js.ws("D"), toJs(p1), toJs(p2)];
      }
    );
  }

  public static function fromJs (js: Js): PropT {
    final a = js.ra();
    return switch (a[0].rs()) {
      case "0": mkAtomic(a[1].rs());
      case "N": mkN(fromJs(a[1]));
      case "C": mkC(fromJs(a[1]), fromJs(a[2]));
      case "A": mkA(fromJs(a[1]), fromJs(a[2]));
      case "K": mkK(fromJs(a[1]), fromJs(a[2]));
      case "E": mkE(fromJs(a[1]), fromJs(a[2]));
      case "D": mkD(fromJs(a[1]), fromJs(a[2]));
      default: throw new Exc("'" + js + "' is not a valid serialized proposition");
    }
  }
}
