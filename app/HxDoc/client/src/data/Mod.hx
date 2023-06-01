// Copyright 23-Jun-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Module data.
package data;

import haxe.ds.Option;
import dm.It;

/// Documentation entry.
class Mentry {
  public var id: String = "????";
  public var link: String = "????";
  public var code: String = "????";
  public var doc: String = "";
  public var line: Int = -1;

  /// Constructor.
  public function new () {}

  /// Ascendent sort.
  public static function sort (es: Array<Mentry>) {
    es.sort((e1, e2) -> e1.id.toUpperCase() > e2.id.toUpperCase() ? 1 : -1);
  }
}

/// Class data.
class Klass extends Mentry {
  public var constructor: Option<Mentry> = None;
  public var ivars: Array<Mentry> = [];
  public var ifuns: Array<Mentry> = [];
  public var svars: Array<Mentry> = [];
  public var sfuns: Array<Mentry> = [];
  public function new () {
    super ();
  }

  /// Rreturns every documentable entry (the same class inclusive).
  public function entries (): It<Mentry> {
    var r = switch (constructor) { case Some(c): [c]; case None: []; };
    return It.from([cast(this)])
      .cat(It.from(r))
      .cat(It.from(ivars))
      .cat(It.from(ifuns))
      .cat(It.from(svars))
      .cat(It.from(sfuns))
    ;
  }

  /// Ascedent sort of every element of 'cs'.
  public static function sort (cs: Array<Klass>) {
    Mentry.sort(cast(cs));
    for (c in cs) {
      Mentry.sort(c.ivars);
      Mentry.sort(c.ifuns);
      Mentry.sort(c.svars);
      Mentry.sort(c.sfuns);
    }
  }
}

/// Module data.
class Mod {
  /// Overview documentation.
  public var overview: Option<String> = None;
  /// Enums documentation.
  public var enums: Array<Mentry> = [];
  /// Typedefs documentation.
  public var typedefs: Array<Mentry> = [];
  /// Classes documentation.
  public var classes: Array<Klass> = [];

  /// Constructor.
  public function new () {}

  /// Returns every documentable element of 'this'.
  public function entries (): It<Mentry> {
    return It.from(typedefs).cat(It.from(enums).cat(It.from(classes).reduce(
      It.empty(), (r, e) -> r.cat(e.entries())
    )));
  }

  /// Ascendente sort of every element of 'this'.
  public function sort () {
    Mentry.sort(enums);
    Mentry.sort(typedefs);
    Klass.sort(classes);
  }
}
