// Copyright 24-Aug-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Server and Sconf records.
package data;

import dm.Opt;
import dm.Js;

/// Server configuration record.
class Sconf {
  public var cmd: String;
  public var url: String;
  public var regex: String;
  public var sel: Int;
  public var isIsoDate: Bool;
  public var dateSeparator: String;
  public var isIsoNumber: Bool;
  public var fieldsType: String;
  public var tableStart: String;
  public var tableEnd: String;
  public var rowStart: String;
  public var rowEnd: String;
  public var cellsStart: Array<String>;
  public var cellsEnd: Array<String>;

  public function new (
    cmd: String,
    url: String,
    regex: String,
    sel: Int,
    isIsoDate: Bool,
    dateSeparator: String,
    isIsoNumber: Bool,
    fieldsType: String,
    tableStart: String,
    tableEnd: String,
    rowStart: String,
    rowEnd: String,
    cellsStart: Array<String>,
    cellsEnd: Array<String>
  ) {
    this.cmd = cmd;
    this.url = url;
    this.regex = regex;
    this.sel = sel;
    this.isIsoDate = isIsoDate;
    this.dateSeparator = dateSeparator;
    this.isIsoNumber = isIsoNumber;
    this.fieldsType = fieldsType;
    this.tableStart = tableStart;
    this.tableEnd = tableEnd;
    this.rowStart = rowStart;
    this.rowEnd = rowEnd;
    this.cellsStart = cellsStart;
    this.cellsEnd = cellsEnd;
  }

  public function toJs (): Js {
    return Js.wa([
      Js.ws(cmd),
      Js.ws(url),
      Js.ws(regex),
      Js.wi(sel),
      Js.wb(isIsoDate),
      Js.ws(dateSeparator),
      Js.wb(isIsoNumber),
      Js.ws(fieldsType),
      Js.ws(tableStart),
      Js.ws(tableEnd),
      Js.ws(rowStart),
      Js.ws(rowEnd),
      Js.wa(cellsStart.map(e -> Js.ws(e))),
      Js.wa(cellsEnd.map(e -> Js.ws(e)))
    ]);
  }

  public static function fromJs (js: Js): Sconf {
    final a = js.ra();
    return new Sconf(
      a[0].rs(),
      a[1].rs(),
      a[2].rs(),
      a[3].ri(),
      a[4].rb(),
      a[5].rs(),
      a[6].rb(),
      a[7].rs(),
      a[8].rs(),
      a[9].rs(),
      a[10].rs(),
      a[11].rs(),
      a[12].ra().map(e -> e.rs()),
      a[13].ra().map(e -> e.rs())
    );
  }
}

class ServerCode {
  public var nickId(default, null): Int;
  public var code: Option<String>;

  public function new (nickId: Int, code: Option<String>) {
    this.nickId = nickId;
    this.code = code;
  }

  public function toJs (): Js {
    return Js.wa([
      Js.wi(nickId),
      switch (code) {
        case None: Js.wn();
        case Some(c): Js.ws(c);
      }
    ]);
  }

  public static function fromJs (js: Js): ServerCode {
    final a = js.ra();
    return new ServerCode(
      a[0].ri(),
      a[1].isNull() ? None : Some(a[1].rs())
    );
  }
}

/// Server record.
class Server {
  public var id(default, null): Int;
  public var shortName: String;
  public var name: String;
  public var dailyConf: Option<Sconf>;
  public var historicConf: Option<Sconf>;
  public var codes(default, null): Array<ServerCode>;

  public function new (
    id: Int,
    shortName: String,
    name: String,
    dailyConf: Option<Sconf>,
    historicConf: Option<Sconf>,
    codes: Array<ServerCode>
  ) {
    this.id = id;
    this.shortName = shortName;
    this.name = name;
    this.dailyConf = dailyConf;
    this.historicConf = historicConf;
    this.codes = codes;
  }

  public function toJs (): Js {
    return Js.wa([
      Js.wi(id),
      Js.ws(shortName),
      Js.ws(name),
      switch (dailyConf) {
        case None: Js.wn();
        case Some(cf): cf.toJs();
      },
      switch (historicConf) {
        case None: Js.wn();
        case Some(cf): cf.toJs();
      },
      Js.wa(codes.map(e -> e.toJs()))
    ]);
  }

  public static function fromJs (js: Js): Server {
    final a = js.ra();
    return new Server(
      a[0].ri(),
      a[1].rs(),
      a[2].rs(),
      a[3].isNull() ? None : Some(Sconf.fromJs(a[3])),
      a[4].isNull() ? None : Some(Sconf.fromJs(a[4])),
      a[5].ra().map(e -> ServerCode.fromJs(e))
    );
  }
}
