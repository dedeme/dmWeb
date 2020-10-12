// Copyright 09-Oct-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Log entry.

package data;

import dm.Js;

// Result of directories test.
class TestRs {
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
    r.withBackups = a[0].rb();
    r.withPathTxt = a[1].rb();
    r.path = a[2].rs();
    r.pathOk = a[3].rb();
    r.notInBase = a[4].rb();
    r.isMissing = a[5].rb();
    r.synchronized = a[6].rb();
    return r;
  }
}
