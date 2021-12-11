// Copyright 08-Nov-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data.rank;

import data.model.Model;
import dm.Js;

// Rankings table entry.
class RangeEntry {
  public final paramId: Int;
  public final value: Float;
  public final sales: Float;
  public final param: Float;

  function new (paramId: Int, value: Float, sales: Float) {
    this.paramId = paramId;
    this.value = value;
    this.sales = sales;
    param = paramId / (Cts.rangesGroupNumber * 100);
  }

  public static function fromJs (js: Js): RangeEntry {
    final a = js.ra();
    return new RangeEntry(a[0].ri(), a[1].rf(), a[2].rf());
  }}
