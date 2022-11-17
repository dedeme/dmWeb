// Copyright 01-Jan-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import dm.Js;

class Conf {
  public final color: String;

  function new (color: String) {
    this.color = color;
  }

  public static function fromJs(js: Js): Conf {
    final a = js.ra();
    return new Conf(
      a[0].rs()
    );
  }
}
