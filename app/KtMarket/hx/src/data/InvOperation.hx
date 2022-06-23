// Copyright 18-Oct-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import dm.Js;

/// Trading operation
class InvOperation {
/// NOTE: If stocks > 0 is a sell operation.
///       If stocks == 0 is a normal buy operation.
///       if stocks < 0 is a rebuy
  public var stocks(default, null): Int;
  public var manager(default, null): Int;
  public var nick(default, null): String;

  function new () {}

  public static function fromJs(js: Js): InvOperation {
    final a = js.ra();
    final r = new InvOperation();
    r.stocks = a[0].ri();
    r.manager = a[1].ri();
    r.nick = a[2].rs();
    return r;
  }
}
