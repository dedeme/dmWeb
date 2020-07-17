// Copyright 11-Jul-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import dm.Js;
import dm.Opt;
import dm.Dt;

enum UserEvalType {
  NONE; UP; DOWN;
}

class NewsEntry {
  public var date(default, null): Date;
  public var text(default, null): String;
  public var source(default, null): String;
  public var author(default, null): String;
  public var url(default, null): String;
  public var seval(default, null): Float;
  public var aeval(default, null): Float;
  public var weval(default, null): Float;
  public var ttEval(default, null): Float;
  public var userEval: UserEvalType;

  function new () {}

  public function toJs(): Js {
    return Js.wa([
      Js.ws(Dt.to(date)),
      Js.ws(text),
      Js.ws(source),
      Js.ws(author),
      Js.ws(url),
      Js.wf(seval),
      Js.wf(aeval),
      Js.wf(weval),
      Js.wf(ttEval),
      Js.wi(switch (userEval) {
          case NONE: 0;
          case UP: 1;
          case DOWN: 2;
        })
    ]);
  }

  public static function fromJs(js: Js): NewsEntry {
    var a = js.ra();
    var r = new NewsEntry();
    r.date = Opt.get(Dt.from(a[0].rs()));
    r.text = a[1].rs();
    r.source = a[2].rs();
    r.author = a[3].rs();
    r.url = a[4].rs();
    r.seval = a[5].rf();
    r.aeval = a[6].rf();
    r.weval = a[7].rf();
    r.ttEval = a[8].rf();
    r.userEval = switch (a[9].ri()) {
        case 0: NONE;
        case 1: UP;
        default: DOWN;
      };
    return r;
  }
}

