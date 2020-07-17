// Copyright 29-Jun-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package wgs;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.It;
import dm.Str;
import data.Acc;
import I18n._;

/// Count selector for Diary and Cash pages.
class AccountSelector {
  var acc: Acc;
  var account: String;
  var fn: String -> String -> Void;
  var forCash: Bool;

  /// Returns a "td"
  public var wg (get, never): Domo;
  final planDiv = Q("div");

  /// Constructor
  ///   acc    : Accunting data.
  ///   account: Selected account.
  ///   fn     : Function(subaccount, description) launched when a subaccount
  ///            is selected.
  ///   forCash: If it is 'true', only searh in accounts used with the main cash
  ///            (Bankia).
  public function new (
    acc: Acc, account: String, fn: String -> String -> Void, forCash = false
  ){
    this.acc = acc;
    this.account = account;
    this.fn = fn;
    this.forCash = forCash;
  }

  // Control -------------------------------------------------------------------

  function changeTo(a: String) {
    account = a.length == 1
      ? It.fromMap(acc.subOf(It.fromMap(acc.subOf(a)).next().e1)).next().e1
      : a.length == 2
        ? It.fromMap(acc.subOf(a)).next().e1
        : a
    ;
    planDiv
      .removeAll()
      .add(planHelpf())
    ;
  }

  // View ----------------------------------------------------------------------

  function planHelpf () {
    return Q("ul")
      .style("list-style:none;padding-left:0px;")
      .adds(It.range(1, 4).map(lg ->
        Q("li")
          .html("<a href='#' onclick='return false;'>" +
            Str.cutRight(
              acc.descriptionOf(account.substring(0, lg)), Cts.helpLen
            ) +
            "</a>")
          .add(Q("ul")
            .att("id", "hlist")
            .style("list-style:none;padding-left:10px;")
            .adds(It.fromMap(acc.subOf(account.substring(0, lg - 1)))
              .sort((tp1, tp2) -> tp1.e1 > tp2.e1 ? 1 : -1)
              .map(tp ->
                Q("li")
                  .add(Ui.link(e -> changeTo(tp.e1))
                    .klass("link")
                    .att("title", tp.e1)
                    .html(Str.cutRight(tp.e2.description, Cts.helpLen)))).to()))
      ).to())
      .add(Q("li")
        .add(Q("hr")))
      .adds(It.fromMap(acc.sub(account))
        .filter(tp -> tp.e1 != Cts.cash || !forCash)
        .sort((tp1, tp2) -> tp1.e1 > tp2.e1 ? 1 : -1)
        .map(tp ->
          Q("li")
            .add(Ui.link(e -> fn(tp.e1, tp.e2.description))
              .klass("link")
              .att("title", Cts.accFormat(tp.e1))
              .html(Str.cutRight(tp.e2.description, Cts.helpLen)))).to())
    ;
  }

  function get_wg () {
    changeTo(account);
    return Q("td")
      .klass("frame")
      .style("width:250px;vertical-align:top;white-space:nowrap")
      .add(Q("p")
        .html("<b>" + _("Most used accounts") + "</b>"))
      .add(Q("ul")
        .style("list-style:none;padding-left:0px;")
        .adds(acc.mostUsedSubaccounts(forCash)
          .map(tp ->
            Q("li")
              .add(Ui.link(e -> fn(tp.e1, tp.e2))
                .klass("link")
                .att("title", Cts.accFormat(tp.e1))
                .text(Str.cutRight(tp.e2, Cts.helpLen)))
          ).to()))
      .add(Q("p")
        .html("<b>" + _("Plan") + "</b>"))
      .add(planDiv)
    ;
  }


}
