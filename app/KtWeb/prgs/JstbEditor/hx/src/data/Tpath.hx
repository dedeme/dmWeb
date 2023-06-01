// Copyright 20-Jan-2023 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

/// Path that defines a table field and its configuration.

package data;

import dm.Js;
import dm.Opt;

class Tpath {
  /// Table path.
  public final table: Table;
  /// Field path
  public final field: Field;
  /// Mutable field configuration
  public var fieldCf: Option<FieldCf>;

  public function new (table: Table, field:Field, fieldCf: Option<FieldCf>) {
    this.table = table;
    this.field = field;
    this.fieldCf = fieldCf;
  }

  public function toJs (): Js {
    return Js.wa([
      table.toJs(),
      field.toJs(),
      switch(fieldCf) { case Some(v): Js.wa([v.toJs()]); case None: Js.wa([]); },
    ]);
  }

  public static function fromJs (js: Js): Tpath {
    final a = js.ra();
    final fCf = a[2].ra();
    return new Tpath(
      Table.fromJs(a[0]),
      Field.fromJs(a[1]),
      fCf.length == 0 ? None : Some(FieldCf.fromJs(fCf[0]))
    );
  }
}
