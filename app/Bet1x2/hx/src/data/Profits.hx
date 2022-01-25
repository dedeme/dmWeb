// Copyright 14-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import dm.Js;

/// Profits data.
class Profits {
  public final description: String;
  public final hits: Int;
  public final fails: Int;
  public final amount: Float;

  public function new (
    description: String, hits: Int, fails: Int, amount: Float
  ) {
    this.description = description;
    this.hits = hits;
    this.fails = fails;
    this.amount = amount;
  }

  public static function fromJs (js: Js): Profits {
    final a = js.ra();
    return new Profits(
      a[0].rs(),
      a[1].ri(),
      a[2].ri(),
      a[3].rf()
    );
  }
}
