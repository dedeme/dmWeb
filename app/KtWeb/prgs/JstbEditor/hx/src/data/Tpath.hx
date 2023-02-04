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
  /// Field configuration
  public final fieldCf: Option<FieldCf>;

  public function new (table: Table, field:Field, fieldCf: Option<FieldCf>) {
    this.table = table;
    this.field = field;
    this.fieldCf = fieldCf;
  }

  public function toJs (): Js {
    return Js.wa([
      table.toJs(),
      field.toJs(),
      switch(fieldCf) { case Some(v): v.toJs(); case None: Js.wn(); },
    ]);
  }

  public static function fromJs (js: Js): Tpath {
    final a = js.ra();
    return new Tpath(
      Table.fromJs(a[0]),
      Field.fromJs(a[1]),
      a[2].isNull() ? None : Some(FieldCf.fromJs(a[2]))
    );
  }
}
