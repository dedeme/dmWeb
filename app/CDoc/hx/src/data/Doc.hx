// Copyright 08-Dic-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package data;

import dm.Js;

/// Module documentation data.
class Doc {
  /// Module documentation.
  public final doc: String;
  /// 'defines' documentation.
  public final defines: Array<DocEntry>;
  /// 'enums' documentation.
  public final enums: Array<DocEntry>;
  /// 'structs' documentation.
  public final structs: Array<DocEntry>;
  /// 'typedefs' documentation.
  public final typedefs: Array<DocEntry>;
  /// 'unions' documentation.
  public final unions: Array<DocEntry>;
  /// Functions documentation.
  public final functions: Array<DocEntry>;
  /// Variables documentation.
  public final vars: Array<DocEntry>;

  function new (
    doc: String, defines: Array<DocEntry>, enums:Array< DocEntry>,
    structs: Array<DocEntry>, typedefs: Array<DocEntry>,
    unions: Array<DocEntry>, functions: Array<DocEntry>, vars: Array<DocEntry>
  ) {
    this.doc = doc;
    this.defines = defines;
    this.enums = enums;
    this.structs = structs;
    this.typedefs = typedefs;
    this.unions = unions;
    this.functions = functions;
    this.vars = vars;
  }

  public function toJs (): Js {
    return Js.wa([
      Js.ws(doc),
      Js.wa(defines.map(e -> e.toJs())),
      Js.wa(enums.map(e -> e.toJs())),
      Js.wa(structs.map(e -> e.toJs())),
      Js.wa(typedefs.map(e -> e.toJs())),
      Js.wa(unions.map(e -> e.toJs())),
      Js.wa(functions.map(e -> e.toJs())),
      Js.wa(vars.map(e -> e.toJs()))
    ]);
  }

  public static function fromJs (js: Js): Doc {
    final a = js.ra();
    return new Doc(
      a[0].rs(),
      a[1].ra().map(DocEntry.fromJs),
      a[2].ra().map(DocEntry.fromJs),
      a[3].ra().map(DocEntry.fromJs),
      a[4].ra().map(DocEntry.fromJs),
      a[5].ra().map(DocEntry.fromJs),
      a[6].ra().map(DocEntry.fromJs),
      a[7].ra().map(DocEntry.fromJs)
    );
  }
}
