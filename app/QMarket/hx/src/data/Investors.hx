// Copyright 03-Nov-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import dm.Js;
import dm.It;

/// Investors data.
class Investors {
  public final base: Int;
  public final list: Map<String, Array<Int>>;

  function new (base: Int, list: Map<String, Array<Int>>) {
    this.base = base;
    this.list = list;
  }

  public static function fromJs (js: Js): Investors {
    final a = js.ra();
    final base = a[0].ri();
    final list = new Map<String, Array<Int>>();
    It.fromMap(a[1].ro()).each(tp -> {
      list[tp.e1] = tp.e2.ra().map(e -> e.ri());
    });
    return new Investors(base, list);
  }
}
