// Copyright 15-Jun-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import dm.Js;
import dm.Dec;

/// Investor data.
class Investor {
  /// Model-parameters by default.
  public var base(default, null): Strategy;
  /// Model-parameters por every company.
  public var nicks(default, null): Map<String, Strategy>;

  function new (base: Strategy, nicks: Map<String, Strategy>) {
    this.base = base;
    this.nicks = nicks;
  }

  public static function fromJs (js: Js): Investor {
    final a = js.ra();
    return new Investor(
      Strategy.fromJs(a[0]),
      a[1].rMap(e -> Strategy.fromJs(e))
    );
  }
}

