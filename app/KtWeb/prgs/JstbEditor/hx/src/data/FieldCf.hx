// Copyright 20-Jan-2023 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Field configuration.
/// (Only for Arrays and Maps);

package data;

import dm.Js;
import dm.Opt;

class IndexKeyFilter {
  public final startRange: Int;
  public final endRange: Int;
  /// Can be FielCf.EQUALS, FielCf.STARTS or FielCf.IN.
  public final searchType: Int;
  public final isUpper: Bool;
  public final text: String;

  function new (
    startRange: Int, endRange: Int,
    searchType: Int, isUpper: Bool, text: String
  ) {
    this.startRange = startRange;
    this.endRange = endRange;
    this.searchType = searchType;
    this.isUpper = isUpper;
    this.text = text;
  }

  public static function mkArray (startRange: Int, endRange: Int) {
    return new IndexKeyFilter(startRange, endRange, 0, false, "");
  }

  public static function mkMap (searchType: Int, isUpper: Bool, text: String) {
    return new IndexKeyFilter(0, 0, searchType, isUpper, text);
  }

  public function toJs (): Js {
    return Js.wa([
      Js.wi(startRange),
      Js.wi(endRange),
      Js.wi(searchType),
      Js.wb(isUpper),
      Js.ws(text)
    ]);
  }

  public static function fromJs (js: Js): IndexKeyFilter {
    final a = js.ra();
    return new IndexKeyFilter(
      a[0].ri(),
      a[1].ri(),
      a[2].ri(),
      a[3].rb(),
      a[4].rs()
    );
  }
}

class FieldCf {
  public static final EQUALS = 0;
  public static final STARTS = 1;
  public static final IN = 2;

  public final indexKeyFilter: Option<IndexKeyFilter>;
  public final valueFilter: Option<String>;

  public function new (
    indexKeyFilter: Option<IndexKeyFilter>, valueFilter: Option<String>
  ) {
    this.indexKeyFilter = indexKeyFilter;
    this.valueFilter = valueFilter;
  }

  public static function mkArray (
    startRange: Int, endRange: Int, valueFilter: Option<String>
  ): FieldCf {
    return new FieldCf(
      Some(IndexKeyFilter.mkArray(startRange, endRange)), valueFilter
    );
  }

  /// Constructor of a Map configuration.
  /// 'searchType' can be EQUALS, STARTS or IN.
  public function mkMap (
    searchType: Int, isUpper: Bool, text: String, valueFilter: Option<String>
  ) {
    return new FieldCf(
      Some(IndexKeyFilter.mkMap(searchType, isUpper, text)), valueFilter
    );
  }

  public function toJs (): Js {
    return Js.wa([
      switch(indexKeyFilter){
          case Some(v): Js.wa([v.toJs()]);
          case None: Js.wa([]);
        },
      switch(valueFilter){
        case Some(v): Js.wa([Js.ws(v)]);
        case None: Js.wa([]);
      }
    ]);
  }

  public static function fromJs (js: Js): FieldCf {
    final a = js.ra();
    final e0 = a[0].ra();
    final e1 = a[1].ra();
    return new FieldCf(
      e0.length == 0 ? None : Some(IndexKeyFilter.fromJs(e0[0])),
      e1.length == 0 ? None : Some(e1[0].rs())
    );
  }
}
