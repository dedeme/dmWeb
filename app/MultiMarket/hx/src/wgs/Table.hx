// Copyright 14-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package wgs;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Ui;
import dm.It;
import dm.Dec;
import dm.Dt;
import dm.Opt;
import data.Cts;
import I18n._;

/// Table for show flea data.
class Table {
  var wg: Domo;
  var cols: Array<Col>;
  var table: Array<Array<Dynamic>>;
  var orderCol: Int;
  var link: (Dynamic, Int) -> String;
  var mainOrder: Bool;

  /// Constructor
  ///   wg      : Container.
  ///   cols    : Table header.
  ///   table   : Table data.
  ///   OrderCol: Number of main column for ordering or -1 if the table is not
  ///             reorderable.
  ///   Link    : Link constructor from table element and column index.
  public function new (
    wg: Domo, cols: Array<Col>, table: Array<Array<Dynamic>>,
    orderCol: Int, link: (Dynamic, Int) -> String
  ) {
    this.wg = wg;
    this.cols = cols;
    this.table = table;
    this.orderCol = orderCol;
    this.link = link;

    mainOrder = orderCol == -1 ? false : cols[orderCol - 1].asc;

    if (orderCol != -1) sortBy(orderCol);
    else view();
  }

  // View ----------------------------------------------------------------------

  function view () {
    if (table.length == 0) {
      wg
        .removeAll()
        .add(Q("table")
          .att("align", "center")
          .add(Q("tr")
            .add(Q("td")
              .klass("frame")
              .html(_("Without data")))));
      return;
    }

    final header: Array<Domo> = [];
    It.from(cols).eachIx((c, i) ->
      header.push(
        c.type == Col.COUNTER
          ? Q("td")
            .klass("header")
            .text(c.title)
          : Q("td")
            .klass("header")
            .add(orderCol == -1
              ? Q("span").text(c.title)
              : Ui.link(e -> sortBy(i + 1))
                .klass("link")
                .text(c.title))
      )
    );

    final rows: Array<Domo> = [];
    It.from(table).eachIx((r, iRow) -> {
      final element = r[0];
      final r2 = It.from(r).drop(1).to();
      final cells: Array<Domo> = [];
      It.from(r2).eachIx((v, i) -> {
        final c = cols[i];
        final t = c.type;
        cells.push(t == Col.COUNTER
          ? Q("td")
            .klass("header")
            .style("text-align:right")
            .text(Dec.toIso(iRow + 1, 0))
          : t == Col.ICON
            ? c.link
              ? Q("td")
                .klass("menu")
                .add(Q("a")
                  .att("href", link(element, i))
                  .add(Ui.img(v)))
              : Q("td")
                .klass("menu")
                .add(Ui.img(v))
            : t == Col.NUMBER || t == Col.PARAM
              ? c.link
                ? Q("td")
                  .klass(t == Col.NUMBER ? "fnumber" : "fparam")
                  .add(Q("a")
                    .att("href", link(element, i))
                    .text(Cts.nformat(cast(v, Float), c.format)))
                : Q("td")
                  .klass(t == Col.NUMBER ? "fnumber" : "fparam")
                  .text(Cts.nformat(cast(v, Float), c.format))
              : t == Col.DATE
                ? Q("td")
                  .klass("menu")
                  .text(Dt.toIso(Opt.oget(Dt.from(v), Date.now())))
                : c.link
                  ? Q("td")
                    .klass(t == Col.STRING ? "menu" : "fparam")
                    .add(Q("a")
                      .att("href", link(element, i))
                      .text(v))
                  : Q("td")
                    .klass(t == Col.STRING ? "menu" : "fparam")
                    .text(v)
        );
      });
      rows.push(Q("tr")
        .adds(cells));
    });

    wg
      .removeAll()
      .add(Q("table").klass("white")
        .add(Q("tr")
          .adds(header))
        .adds(rows))
    ;
  }

  // Control -------------------------------------------------------------------

  function sortBy (iCol: Int) {
    final cols = cols;
    if (iCol == orderCol) {
      table.sort((r1, r2) ->
        cols[iCol - 1].asc
          ? r1[iCol] > r2[iCol] ? 1 : -1
          : r1[iCol] < r2[iCol] ? 1 : -1
      );
      cols[iCol - 1].asc = !cols[iCol - 1].asc;
    } else {
      table.sort((r1, r2) ->
        cols[iCol - 1].asc
          ? r1[iCol] > r2[iCol]
            ? 1
            : r1[iCol] < r2[iCol]
              ? -1
              : mainOrder
                ? r1[orderCol] > r2[orderCol] ? 1 : -1
                : r1[orderCol] < r2[orderCol] ? 1 : -1
          : r1[iCol] < r2[iCol] ? 1
            : r1[iCol] > r2[iCol]
              ? -1
              : mainOrder
                ? r1[orderCol] > r2[orderCol] ? 1 : -1
                : r1[orderCol] < r2[orderCol] ? 1 : -1
      );
      cols[iCol - 1].asc = !cols[iCol - 1].asc;
    }
    this.view();
  }


}

/// Table column header.
class Col {
  public var title(default, null): String;
  /// One of NUMBER, PARAM, STRING, P_STRING, COUNTER, ICON or DATE.
  public var type(default, null): Int;
  /// expect next values:
  ///   4, 6 : Percentage format (Dec.toIso(v*100, [2|4]) + "%").
  ///   other: Number format (Dec.toIso(v, 'other')).
  public var format(default, null): Int;
  /// If value is a link.
  public var link(default, null): Bool;
  /// If ascendent order is used.
  public var asc: Bool;

  public function new (
    title: String, type: Int, format: Int, link: Bool, asc: Bool
  ) {
    this.title = title;
    this.type = type;
    this.format = format;
    this.link = link;
    this.asc = asc;
  }

  public static final  NUMBER = 0;
  public static final  PARAM = NUMBER + 1;
  public static final  STRING = PARAM + 1;
  public static final  P_STRING = STRING + 1;
  public static final  COUNTER = P_STRING + 1;
  public static final  ICON = COUNTER + 1;
  public static final  DATE = ICON + 1;
}
