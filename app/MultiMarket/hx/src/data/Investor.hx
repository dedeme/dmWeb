// Copyright 18-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import dm.Js;
import dm.It;
import dm.Dec;
import data.flea.Fmodel;

/// Manager data.
class Investor {
  /// Model-parameters by default.
  public var base(default, null): InvestorEntry;
  /// Model-parameters por every company.
  public var nicks(default, null): Map<String, InvestorEntry>;

  function new (base: InvestorEntry, nicks: Map<String, InvestorEntry>) {
    this.base = base;
    this.nicks = nicks;
  }

  public static function fromJs (js: Js): Investor {
    final a = js.ra();
    return new Investor(
      InvestorEntry.fromJs(a[0]),
      a[1].rMap(e -> InvestorEntry.fromJs(e))
    );
  }
}

/// Manager entry
class InvestorEntry {
  /// Flea model.
  public var model(default, null): Fmodel;
  /// Flea parameters.
  public var param(default, null): Float;

  function new (model: Fmodel, param: Float) {
    this.model = model;
    this.param = param;
  }

  /// Returns true if parameneters of this are equals to other ones.
  public function eqParam(other: InvestorEntry): Bool {
    return model.id == model.id &&
      Dec.eq(param, other.param, 0.0000001)
    ;
  }

  public static function fromJs(js: Js): InvestorEntry {
    final a = js.ra();
    return new InvestorEntry(
      Fmodel.fromJs(a[0]),
      a[1].rf()
    );
  }
}
