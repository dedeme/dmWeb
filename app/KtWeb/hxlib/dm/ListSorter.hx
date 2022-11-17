// Copyright 26-Aug-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package dm;

import dm.Domo;

/// Creates a widget to sort lists. For example:
///
/// ---- Create a horizontal widget.
///    final ls = new LS<Int>(
///      () -> Ui.img("blank"),
///      () -> Ui.img("go-previous"),
///      () -> Ui.img("go-next"),
///      listA,
///      l -> {
///        listA = l;
///        updateA();
///      }
///    );
///    ...
///    listADiv.removeAll().add(
///      Q("table")
///        .add(Q("tr")
///          .adds(ls.ups.map(e -> Q("td").add(e))))
///        .add(Q("tr")
///          .adds(listA.map(e -> Q("td").html(Std.string(e)))))
///        .add(Q("tr")
///          .adds(ls.downs.map(e -> Q("td").add(e))))
///    );
///
/// ---- Create a vertical widget.
///    final ls = new LS<Tp<Int, String>>(
///      () -> Ui.img("blank"),
///      () -> Ui.img("go-up"),
///      () -> Ui.img("go-down"),
///      listB,
///      l -> {
///        listB = l;
///        updateB();
///      }
///    );
///    var i = 0;
///    listBDiv.removeAll().add(
///      Q("table")
///        .adds(listB.map(e -> {
///          final r = Q("tr")
///            .add(Q("td").add(ls.ups[i]))
///            .add(Q("td").add(ls.downs[i]))
///            .add(Q("td").html(Std.string(e.e1)))
///            .add(Q("td").html(e.e2));
///          i++;
///          return r;
///        }))
///    );
class ListSorter<T> {
  /// Up arrrows widget (including blank arrow)
  public var ups(default, null): Array<Domo>;
  /// Down arrrows widget (including blank arrow)
  public var downs(default, null): Array<Domo>;

  /// Constructor.
  /// mkBlank: Constructor of empty arrow.
  /// mkUp: Constructor of up (left) arrow.
  /// mkDown: Constructor of down (right) arrow.
  /// action: Action to do when an arrow is clicked.
  public function new (
    mkBlank: () -> Domo,
    mkUp: () -> Domo,
    mkDown: () -> Domo,
    list: Array<T>,
    action: Array<T> -> Void
  ) {
    final len1 = list.length - 1;
    if (list.length < 1)
      throw "list must have at least 2 elements";

    final l = list.copy();
    ups = [];
    downs = [];

    for (i in 0...len1+1) {
      if (i == 0) {
        ups.push(mkBlank());
      } else {
        ups.push(mkUp().on(CLICK, e -> {
          final tmp = l[i];
          l[i] = l[i - 1];
          l[i - 1] = tmp;
          action(l);
        }));
      }
      if (i == len1) {
        downs.push(mkBlank());
      } else {
        downs.push(mkDown().on(CLICK, e -> {
          final tmp = l[i];
          l[i] = l[i + 1];
          l[i + 1] = tmp;
          action(l);
        }));
      }
    }
  }
}

