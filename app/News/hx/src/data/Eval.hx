// Copyright 11-Jul-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import dm.Js;
import dm.Opt;
import dm.Dt;

class Eval {
  public var id(default, null): String;
  public var eval(default, null): Float;

  function new () {}

  public static function fromJs(js: Js): Eval {
    var a = js.ra();
    var r = new Eval();
    r.id = a[0].rs();
    r.eval = a[1].rf();
    return r;
  }
}

