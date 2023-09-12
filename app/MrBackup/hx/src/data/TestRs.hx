// Copyright 09-Oct-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Log entry.

package data;

import dm.Js;

// Result of directories test.
class TestRs {
  public var isBig(default, null): Bool;
  public var withBackups(default, null): Bool;
  public var withPathTxt(default, null): Bool;
  public var path(default, null): String;
  public var pathOk(default, null): Bool;
  public var notInBase(default, null): Bool;
  public var isMissing(default, null): Bool;
  public var synchronized(default, null): Bool;

  function new () {}

  public static function FromJs (js: Js): TestRs {
    final r = new TestRs();
    final a = js.ra();
    r.isBig = a[0].rb();
    r.withBackups = a[1].rb();
    r.withPathTxt = a[2].rb();
    r.path = a[3].rs();
    r.pathOk = a[4].rb();
    r.notInBase = a[5].rb();
    r.isMissing = a[6].rb();
    r.synchronized = a[7].rb();
    return r;
  }
}
