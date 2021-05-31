// Copyright 18-May-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pages;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import I18n._;

/// Times configuration.
class Times {
  final wg: Domo;
  final pict: Int;
  final shortDance: Int;
  final longDance: Int;
  final fnBack: Void -> Void;

  function new (
    wg: Domo,
    pict: Int, shortDance: Int, longDance: Int,
    fnBack: Void -> Void
  ) {
    this.wg = wg;
    this.pict = pict;
    this.shortDance = shortDance;
    this.longDance = longDance;
    this.fnBack = fnBack;

    view();
  }

  // VIEW

  function view () {
    function td (n: Int): Domo {
      return Q("td")
        .style("text-align:center")
        .text(Std.string(n))
      ;
    }

    function op(group: String, value: Int, sel: Int): Domo {
      return Q("td")
        .style("text-align:center")
        .add(Q("input")
          .att("type", "radio")
          .att("name", group)
          .checked(value == sel)
          .on(CLICK, () -> send(group, value)))
      ;
    }

    wg
      .removeAll()
      .add(Q("div")
        .klass("head")
        .text(_("Times Management")))
      .add(Q("table")
        .att("align", "center")
        .add(Q("tr")
          .add(Q("td")
            .add(Ui.link(fnBack)
              .klass("link")
              .text("[ " + _("Back") + " ]")))))
      .add(Q("table")
        .att("align", "center")
        .add(Q("tr")
          .add(Q("td")
            .add(Ui.hrule(_("Picture"), 20))))
        .add(Q("tr")
          .add(Q("td")
            .klass("frame")
            .add(Q("table")
              .add(Q("tr")
                .add(op("pict", 1, pict))
                .add(op("pict", 2, pict))
                .add(op("pict", 3, pict))
                .add(op("pict", 4, pict))
                .add(op("pict", 5, pict))
                .add(op("pict", 6, pict)))
              .add(Q("tr")
                .add(td(1))
                .add(td(2))
                .add(td(3))
                .add(td(4))
                .add(td(5))
                .add(td(6)))
              .add(Q("tr")
                .add(op("pict", 10, pict))
                .add(op("pict", 12, pict))
                .add(op("pict", 15, pict))
                .add(op("pict", 20, pict))
                .add(op("pict", 30, pict))
                .add(op("pict", 60, pict)))
              .add(Q("tr")
                .add(td(10))
                .add(td(12))
                .add(td(15))
                .add(td(20))
                .add(td(30))
                .add(td(60))))))
        .add(Q("tr")
          .add(Q("td")
            .add(Ui.hrule(_("Short Dance"), 20))))
        .add(Q("tr")
          .add(Q("td")
            .klass("frame")
            .add(Q("table")
              .add(Q("tr")
                .add(op("shortDance", 10, shortDance))
                .add(op("shortDance", 15, shortDance))
                .add(op("shortDance", 20, shortDance))
                .add(op("shortDance", 25, shortDance))
                .add(op("shortDance", 30, shortDance)))
              .add(Q("tr")
                .add(td(10))
                .add(td(15))
                .add(td(20))
                .add(td(25))
                .add(td(30))))))
        .add(Q("tr")
          .add(Q("td")
            .add(Ui.hrule(_("Long Dance"), 20))))
        .add(Q("tr")
          .add(Q("td")
            .klass("frame")
            .add(Q("table")
              .add(Q("tr")
                .add(op("longDance", 30, longDance))
                .add(op("longDance", 45, longDance))
                .add(op("longDance", 60, longDance))
                .add(op("longDance", 75, longDance))
                .add(op("longDance", 90, longDance)))
              .add(Q("tr")
                .add(td(30))
                .add(td(45))
                .add(td(60))
                .add(td(75))
                .add(td(90)))))))
    ;
  }

  // CONTROL

  function send (group: String, value: Int): Void {
    Cts.client.ssend([
      "source" => Js.ws("Times"),
      "rq" => Js.ws("update"),
      "key" => Js.ws(group),
      "value" => Js.wi(value)
    ], rp -> {
    });
  }

  // STATIC

  public static function mk (wg: Domo, fnBack: Void -> Void): Void {
    Cts.client.ssend([
      "source" => Js.ws("Times"),
      "rq" => Js.ws("idata")
    ], rp -> {
      final pict = rp["pict"].ri(); // minutes
      final shortDance = rp["shortDance"].ri(); // minutes
      final longDance = rp["longDance"].ri(); // minutes

      new Times(
        wg,
        pict, shortDance, longDance,
        fnBack
      );
    });
  }
}
