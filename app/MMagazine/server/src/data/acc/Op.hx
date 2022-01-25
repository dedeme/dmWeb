// Copyright 14-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data.acc;

import dm.Js;

/// Accounting operation.
class Op {
  public static function toJs (op: OpType): Js {
    return switch (op) {
      case OpSe(nick, stocks, price): Js.wa([
          Js.ws("se"),
          Js.ws(nick),
          Js.wi(stocks),
          Js.wf(price)
        ]);
      case OpBu(nick, stocks, price): Js.wa([
          Js.ws("bu"),
          Js.ws(nick),
          Js.wi(stocks),
          Js.wf(price)
        ]);
      case OpSt(nick, stocks, price): Js.wa([
          Js.ws("st"),
          Js.ws(nick),
          Js.wi(stocks),
          Js.wf(price)
        ]);
      case OpIn(amount): Js.wa([
          Js.ws("in"),
          Js.wf(amount)
        ]);
      case OpWi(amount): Js.wa([
          Js.ws("wi"),
          Js.wf(amount)
        ]);
      case OpPr(amount, cause): Js.wa([
          Js.ws("pr"),
          Js.wf(amount),
          Js.ws(cause)
        ]);
      case OpFe(amount, cause): Js.wa([
          Js.ws("fe"),
          Js.wf(amount),
          Js.ws(cause)
        ]);
      case OpPd(amount, cause): Js.wa([
          Js.ws("pd"),
          Js.wf(amount),
          Js.ws(cause)
        ]);
      case OpNd(amount, cause): Js.wa([
          Js.ws("nd"),
          Js.wf(amount),
          Js.ws(cause)
        ]);
    }
  }

  public static function fromJs (js: Js): OpType {
    final a = js.ra();
    return switch (a[0].rs()) {
      case "se": OpSe(a[1].rs(), a[2].ri(), a[3].rf());
      case "bu": OpBu(a[1].rs(), a[2].ri(), a[3].rf());
      case "st": OpSt(a[1].rs(), a[2].ri(), a[3].rf());
      case "in": OpIn(a[1].rf());
      case "wi": OpWi(a[1].rf());
      case "pr": OpPr(a[1].rf(), a[2].rs());
      case "fe": OpFe(a[1].rf(), a[2].rs());
      case "pd": OpPd(a[1].rf(), a[2].rs());
      case "nd": OpNd(a[1].rf(), a[2].rs());
      case s : throw new haxe.Exception('Unexpected operation type "${s}".');
    }
  }
}

enum OpType {
	// sell
	OpSe (nick: String, stocks: Int, price: Float);
	// buy
	OpBu (nick: String, stocks: Int, price: Float);
	// Stock
	OpSt (nick: String, stocks: Int, price: Float);
	// Income
	OpIn (amount: Float);
	// Withdrawal
	OpWi (amount: Float);
	// Profits
	OpPr (amount: Float, cause: String);
	// Fees
	OpFe (amount: Float, cause: String);
	// Positive differences
	OpPd (amount: Float, cause: String);
	// Negative differences
	OpNd (amount: Float, cause: String);
}
