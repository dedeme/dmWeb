// Copyright 16-Nov-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Accounting annotation entry

package data;

import dm.Js;

class AccAnn {
  final date: String;
  final description: String;
  final amount: Float;

  public function new (date: String, description: String, amount: Float) {
    this.date = date;
    this.description = description;
    this.amount = amount;
  }

  public static function fromJs (js: Js): AccAnn {
    final a = js.ra();
    return new AccAnn(
      a[0].rs(),
      a[1].rs(),
      a[2].rf()
    );
  }
}
