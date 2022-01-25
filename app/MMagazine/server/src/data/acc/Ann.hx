// Copyright 14-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data.acc;

import dm.Js;
import dm.It;
import data.acc.Op;

/// Annotation data.
class Ann {
  public final id: Int;
  public final date: String;
  public final op: OpType;

  public function new (id: Int, date: String, op: OpType) {
    this.id = id;
    this.date = date;
    this.op = op;
  }

  public function toJs (): Js {
    return Js.wa([
      Js.wi(id),
      Js.ws(date),
      Op.toJs(op)
    ]);
  }

  public static function fromJs (js: Js): Ann {
    final a = js.ra();
    final subJs = Js.wa(It.from(a).drop(2).to());
    return new Ann(
      a[0].ri(),
      a[1].rs(),
      Op.fromJs(subJs)
    );
  }
}

