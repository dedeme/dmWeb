// Copyright 07-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import dm.Js;
import dm.It;

/// Accounting annotation.
class Annotation {
  public var id(default, null): Int;
  public var date(default, null): String;
  public var operation(default, null): Operation;

  public function new(id: Int, date: String, operation: Operation) {
    this.id = id;
    this.date = date;
    this.operation = operation;
  }

  public function toJs (): Js {
    return Js.wa(
      It.from([
        Js.wi(id),
        Js.ws(date)
      ]).cat(
        It.from(operation.toJs().ra())
      ).to()
    );
  }

  public static function fromJs (js: Js): Annotation {
    final a = js.ra();
    return new Annotation(
      a[0].ri(),
      a[1].rs(),
      Operation.fromJs(Js.wa(It.from(a).drop(2).to()))
    );
  }
}

class Operation {
  // sell
  static final annSe = 0;
	// buy
	static final annBu = annSe + 1;
	// Stock
	static final annSt = annBu + 1;
	// Income
	static final annIn = annSt + 1;
	// Withdrawal
	static final annWi = annIn + 1;
	// Profits
	static final annPr = annWi + 1;
	// Fees
	static final annFe = annPr + 1;
	// Positive differences
	static final annPd = annFe + 1;
	// Negative differences
	static final annNd = annPd + 1;

  var tp: Int;
  var i: Int;
  var f: Float;
  var s: String;

  function new (tp: Int, i: Int, f: Float, s: String) {
    this.tp = tp;
    this.i = i;
    this.f = f;
    this.s = s;
  }

  public static function mkSe(
    nick: String, stocks: Int, price: Float
  ): Operation {
    return new Operation(Operation.annSe, stocks, price, nick);
  }

  public static function mkBu(
    nick: String, stocks: Int, price: Float
  ): Operation {
    return new Operation(Operation.annBu, stocks, price, nick);
  }

  public static function mkSt(
    nick: String, stocks: Int, price: Float
  ): Operation {
    return new Operation(Operation.annSt, stocks, price, nick);
  }

  public static function mkPr(amount: Float, cause: String): Operation {
    return new Operation(Operation.annPr, 0, amount, cause);
  }

  public static function mkFe(amount: Float, cause: String): Operation {
    return new Operation(Operation.annFe, 0, amount, cause);
  }

  public static function mkPd(amount: Float, cause: String): Operation {
    return new Operation(Operation.annPd, 0, amount, cause);
  }

  public static function mkNd(amount: Float, cause: String): Operation {
    return new Operation(Operation.annNd, 0, amount, cause);
  }

  public static function mkIn(amount: Float): Operation {
    return new Operation(Operation.annIn, 0, amount, "");
  }

  public static function mkWi(amount: Float): Operation {
    return new Operation(Operation.annWi, 0, amount, "");
  }

  public function se(): {nick: String, stocks: Int, price: Float, ok: Bool} {
    return {nick: s, stocks: i, price: f, ok: tp == annSe}
  }

  public function bu(): {nick: String, stocks: Int, price: Float, ok: Bool} {
    return {nick: s, stocks: i, price: f, ok: tp == annBu}
  }

  public function st(): {nick: String, stocks: Int, price: Float, ok: Bool} {
    return {nick: s, stocks: i, price: f, ok: tp == annSt}
  }

  public function pr(): {amount: Float, cause: String, ok: Bool} {
    return {amount: f, cause: s, ok: tp == annPr}
  }

  public function fe(): {amount: Float, cause: String, ok: Bool} {
    return {amount: f, cause: s, ok: tp == annFe}
  }

  public function pd(): {amount: Float, cause: String, ok: Bool} {
    return {amount: f, cause: s, ok: tp == annPd}
  }

  public function nd(): {amount: Float, cause: String, ok: Bool} {
    return {amount: f, cause: s, ok: tp == annNd}
  }

  public function inc(): {amount: Float, ok: Bool} {
    return {amount: f, ok: tp == annIn}
  }

  public function wi(): {amount: Float, ok: Bool} {
    return {amount: f, ok: tp == annWi}
  }

  public function toJs (): Js {
    return Js.wa(
      switch (tp) {
        case annSe: [Js.ws("se"), Js.ws(s), Js.wi(i), Js.wf(f)];
        case annBu: [Js.ws("bu"), Js.ws(s), Js.wi(i), Js.wf(f)];
        case annSt: [Js.ws("st"), Js.ws(s), Js.wi(i), Js.wf(f)];
        case annPr: [Js.ws("pr"), Js.wf(f), Js.ws(s)];
        case annFe: [Js.ws("fe"), Js.wf(f), Js.ws(s)];
        case annPd: [Js.ws("pd"), Js.wf(f), Js.ws(s)];
        case annNd: [Js.ws("nd"), Js.wf(f), Js.ws(s)];
        case annIn: [Js.ws("in"), Js.wf(f)];
        default: [Js.ws("wi"), Js.wf(f)];
      }
    );
  }

  public static function fromJs(js: Js): Operation {
    final a = js.ra();
    switch (a[0].rs()) {
      case "se":
        return mkSe(a[1].rs(), a[2].ri(), a[3].rf());
      case "bu":
        return mkBu(a[1].rs(), a[2].ri(), a[3].rf());
      case "st":
        return mkSt(a[1].rs(), a[2].ri(), a[3].rf());
      case "pr":
        return mkPr(a[1].rf(), a[2].rs());
      case "fe":
        return mkFe(a[1].rf(), a[2].rs());
      case "pd":
        return mkPd(a[1].rf(), a[2].rs());
      case "nd":
        return mkNd(a[1].rf(), a[2].rs());
      case "in":
        return mkIn(a[1].rf());
      case "wi":
        return mkWi(a[1].rf());
    }

    throw("Unkown operation '" + a[0].rs() + "'");
  }
}
