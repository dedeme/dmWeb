// Copyright 05-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs.settings.acc;

using StringTools;
import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Opt;
import dm.Js;
import dm.It;
import dm.Dt;
import dm.Dec;
import dm.DatePicker;
import dm.Menu;
import data.Cts;
import data.Annotation;
import I18n._;
import I18n._args;
import pgs.settings.acc.wgs.Annotations;

private class Field {
  public var name(default, null): String;
  public var wg(default, null): Domo;
  public var width(default, null): Int;
  public function new (name: String, wg: Domo, width: Int) {
    this.name = name;
    this.wg = wg;
    this.width = width;
  }
}

/// Annotations editor page.
class Editor {
  var wg: Domo;
  var year: String;
  var investorId: Int;
  var anns: Array<Annotation>;
  var cash: Float;

  final body = Q("div");
  final editor = Q("div");

  function new (
    wg: Domo, year: String, investorId: Int, anns: Array<Annotation>,
    cash: Float
  ) {
    this.wg = wg;
    this.year = year;
    this.investorId = investorId;
    this.anns = anns;
    this.cash = cash;

    view();
  }

  // View ----------------------------------------------------------------------

  function datePicker (txf: Domo): Domo {
    final dt = new DatePicker();
    dt.action = s -> {};
    dt.lang = "es";
    final r = dt.mkText(txf);
    txf.style("width:90px");
    txf.value("");
    return r;
  }

  function typeDate (tp: String, dt: Domo): Domo {
    return Q("tr").add(Q("td")
      .add(Q("table").klass("main")
        .add(Q("tr")
          .add(Q("td").klass("frame2")
            .style("text-align:center;vertical-align:bottom;")
            .html("<big><b>" + tp + "</big></b>"))
          .add(Q("td")
            .add(Q("table").att("align", "right")
              .add(Q("tr")
                .add(Q("td").style("text-align:center;").html(_("Date"))))
              .add(Q("tr")
                .add(Q("td").add(dt))))))))
    ;
  }

  function cancelAccept (f: Dynamic -> Void): Domo {
    final lk = (tx, f) -> Ui.link(f).klass("link").html(tx);
    return Q("tr")
      .add(Q("td").style("text-align:right;")
        .add(lk(_("Cancel"), e -> {
          editor.removeAll().add(this.selector());
        }))
        .add(Q("span").html("&nbsp;&nbsp;&nbsp;"))
        .add(lk(_("Accept"), f)))
    ;
  }

  function fields (fs: Array<Field>): Domo {
    return Q("table").klass("main")
      .add(Q("tr")
        .adds(fs.map(e -> Q("td").style("text-align:center;").html(e.name))))
      .add(Q("tr")
        .adds(fs.map(e -> Q("td").style("text-align:center;")
          .add(e.wg.style('width:${e.width}px')))))
    ;
  }

  function sell () {
    final v = field -> cast(field.getValue(), String).trim();
    final mkin = () -> Q("input").att("type", "text");
    final dtf = mkin();
    final nkf = mkin();
    final stf = mkin();
    final prf = mkin();
    Ui.changePoint(prf);
    final f = e -> {
      var r = controlBase(_("Date"), v(dtf));
      if (r == "") r = controlBase(_("Nicks"), v(nkf));
      if (r == "") r = controlBase(_("Stocks"), v(stf));
      if (r == "") r = controlInt(_("Stocks"), v(stf));
      if (r == "") r = controlBase(_("Price"), v(prf));
      if (r == "") r = controlBuyPrice("es", _("Price"), v(prf));
      if (r == "") {
        sendAnn(
          "se", v(dtf), v(nkf), Std.parseInt(v(stf)),
          Opt.get(Dec.fromIso(v(prf)))
        );
      } else {
        Ui.alert(r);
      }
    };
    final wg = () -> {
      return Q("table").klass("main")
        .add(typeDate(_("Sell"), this.datePicker(dtf)))
        .add(fields([
          new Field(_("Nick"), nkf, 50),
          new Field(_("Stocks"), stf, 40),
          new Field(_("Price"), prf, 75)
        ]))
        .add(cancelAccept(f))
      ;
    };
    editor.removeAll().add(wg());
  }

  function buy () {
    final v = field -> cast(field.getValue(), String).trim();
    final mkin = () -> Q("input").att("type", "text");
    final dtf = mkin();
    final nkf = mkin();
    final stf = mkin();
    final prf = mkin();
    Ui.changePoint(prf);
    final f = e -> {
      var r = controlBase(_("Date"), v(dtf));
      if (r == "") r = controlBase(_("Nicks"), v(nkf));
      if (r == "") r = controlBase(_("Stocks"), v(stf));
      if (r == "") r = controlInt(_("Stocks"), v(stf));
      if (r == "") r = controlBase(_("Price"), v(prf));
      if (r == "") r = controlBuyPrice("es", _("Price"), v(prf));
      if (r == "") {
        sendAnn(
          "bu", v(dtf), v(nkf), Std.parseInt(v(stf)),
          Opt.get(Dec.fromIso(v(prf)))
        );
      } else {
        Ui.alert(r);
      }
    };
    final wg = () -> {
      return Q("table").klass("main")
        .add(typeDate(_("Buy"), datePicker(dtf)))
        .add(fields([
          new Field(_("Nick"), nkf, 50),
          new Field(_("Stocks"), stf, 40),
          new Field(_("Price"), prf, 75)
        ]))
        .add(cancelAccept(f))
      ;
    };
    editor.removeAll().add(wg());
  }

  function income () {
    final v = field -> cast(field.getValue(), String).trim();
    final mkin = () -> Q("input").att("type", "text");
    final dtf = mkin();
    final amf = mkin();
    Ui.changePoint(amf);
    final f = e -> {
      var r = controlBase(_("Date"), v(dtf));
      if (r == "") r = controlBase(_("Ammount"), v(amf));
      if (r == "") r = controlDouble("es", _("Ammount"), v(amf));
      if (r == "") {
        sendAnn("in", v(dtf), "", 0, Opt.get(Dec.fromIso(v(amf))));
      } else {
        Ui.alert(r);
      }
    };
    final wg = () -> {
      return Q("table").klass("main")
        .add(typeDate(_("Income"), datePicker(dtf)))
        .add(fields([new Field(_("Ammount"), amf, 75)]))
        .add(cancelAccept(f))
      ;
    };
    editor.removeAll().add(wg());
  }

  function withdrawal () {
    final v = field -> cast(field.getValue(), String).trim();
    final mkin = () -> Q("input").att("type", "text");
    final dtf = mkin();
    final amf = mkin();
    Ui.changePoint(amf);
    final f = e -> {
      var r = controlBase(_("Date"), v(dtf));
      if (r == "") r = controlBase(_("Ammount"), v(amf));
      if (r == "") r = controlDouble("es", _("Ammount"), v(amf));
      if (r == "") {
        sendAnn(
          "wi", v(dtf), "", 0, Opt.get(Dec.fromIso(v(amf))));
      } else {
        Ui.alert(r);
      }
    };
    final wg = () -> {
      return Q("table").klass("main")
        .add(typeDate(_("Withdrawal"), datePicker(dtf)))
        .add(fields([new Field(_("Ammount"), amf, 75)]))
        .add(cancelAccept(f))
      ;
    };
    editor.removeAll().add(wg());
  }

  function profits () {
    final v = field -> cast(field.getValue(), String).trim();
    final mkin = () -> Q("input").att("type", "text");
    final dtf = mkin();
    final amf = mkin();
    Ui.changePoint(amf);
    final def = mkin();
    final f = e -> {
      var r = controlBase(_("Date"), v(dtf));
      if (r == "") r = controlBase(_("Ammount"), v(amf));
      if (r == "") r = controlDouble("es", _("Ammount"), v(amf));
      if (r == "") r = controlBase(_("Description"), v(def));
      if (r == "") {
        sendAnn("pr", v(dtf), v(def), 0, Opt.get(Dec.fromIso(v(amf))));
      } else {
        Ui.alert(r);
      }
    };
    final wg = () -> {
      return Q("table").klass("main")
        .add(typeDate(_("Profits"), datePicker(dtf)))
        .add(fields([
          new Field(_("Ammount"), amf, 75),
          new Field(_("Description"), def, 220)
        ]))
        .add(cancelAccept(f))
      ;
    };
    editor.removeAll().add(wg());
  }

  function fees () {
    final v = field -> cast(field.getValue(), String).trim();
    final mkin = () -> Q("input").att("type", "text");
    final dtf = mkin();
    final amf = mkin();
    Ui.changePoint(amf);
    final def = mkin();
    final f = e -> {
      var r = controlBase(_("Date"), v(dtf));
      if (r == "") r = controlBase(_("Ammount"), v(amf));
      if (r == "") r = controlDouble("es", _("Ammount"), v(amf));
      if (r == "") r = controlBase(_("Description"), v(def));
      if (r == "") {
        sendAnn(
          "fe", v(dtf), v(def), 0, Opt.get(Dec.fromIso(v(amf))));
      } else {
        Ui.alert(r);
      }
    };
    final wg = () -> {
      return Q("table").klass("main")
        .add(typeDate(_("Fees"), datePicker(dtf)))
        .add(fields([
          new Field(_("Ammount"), amf, 75),
          new Field(_("Description"), def, 220)
        ]))
        .add(cancelAccept(f))
      ;
    };
    editor.removeAll().add(wg());
  }

  function diffP () {
    final v = field -> cast(field.getValue(), String).trim();
    final mkin = () -> Q("input").att("type", "text");
    final dtf = mkin();
    final amf = mkin();
    Ui.changePoint(amf);
    final def = mkin();
    final f = e -> {
      var r = controlBase(_("Date"), v(dtf));
      if (r == "") r = controlBase(_("Ammount"), v(amf));
      if (r == "") r = controlDouble("es", _("Ammount"), v(amf));
      if (r == "") r = controlBase(_("Description"), v(def));
      if (r == "") {
        sendAnn(
          "pd", v(dtf), v(def), 0, Opt.get(Dec.fromIso(v(amf))));
      } else {
        Ui.alert(r);
      }
    };
    final wg = () -> {
      return Q("table").klass("main")
        .add(typeDate(_("Diff. +"), datePicker(dtf)))
        .add(fields([
          new Field(_("Ammount"), amf, 75),
          new Field(_("Description"), def, 220)
        ]))
        .add(cancelAccept(f))
      ;
    };
    editor.removeAll().add(wg());
  }

  function diffN () {
    final v = field -> cast(field.getValue(), String).trim();
    final mkin = () -> Q("input").att("type", "text");
    final dtf = mkin();
    final amf = mkin();
    Ui.changePoint(amf);
    final def = mkin();
    final f = e -> {
      var r = controlBase(_("Date"), v(dtf));
      if (r == "") r = controlBase(_("Ammount"), v(amf));
      if (r == "") r = controlDouble("es", _("Ammount"), v(amf));
      if (r == "") r = controlBase(_("Description"), v(def));
      if (r == "") {
        sendAnn(
          "nd", v(dtf), v(def), 0, Opt.get(Dec.fromIso(v(amf))));
      } else {
        Ui.alert(r);
      }
    };
    final wg = () -> {
      return Q("table").klass("main")
        .add(typeDate(_("Diff. -"), datePicker(dtf)))
        .add(fields([
          new Field(_("Ammount"), amf, 75),
          new Field(_("Description"), def, 220)
        ]))
        .add(cancelAccept(f))
      ;
    };
    editor.removeAll().add(wg());
  }

  function close () {
    final v = field -> cast(field.getValue(), String).trim();
    final mkin = () -> Q("input").att("type", "text");
    final dtf = mkin();
    final f = e -> {
      final r = controlBase(_("Date"), v(dtf));
      if (r == "") {
        sendAnn("cl", v(dtf), "", 0, 0.0);
      } else {
        Ui.alert(r);
      }
    };
    final wg = () -> {
      return Q("table").klass("main")
        .add(typeDate(_("Close"), datePicker(dtf)))
        .add(cancelAccept(f))
      ;
    };
    editor.removeAll().add(wg());
  }

  function selector () {
    final td = () -> Q("td").style("width:50%;");
    final lk = (tx, f) -> Ui.link(f).klass("link").html(tx);
    return Q("table").klass("main")
      .add(Q("tr")
        .add(td().add(lk(_("Sell"), e -> sell())))
        .add(td().add(lk(_("Buy"), e -> buy()))))
      .add(Q("tr")
        .add(td().add(lk(_("Income"), e -> income())))
        .add(td().add(lk(_("Withdrawal"), e -> withdrawal()))))
      .add(Q("tr")
        .add(td().add(lk(_("Profits"), e -> profits())))
        .add(td().add(lk(_("Fees"), e -> fees()))))
      .add(Q("tr")
        .add(td().add(lk(_("Diff. +"), e -> diffP())))
        .add(td().add(lk(_("Diff. -"), e -> diffN()))))
    ;
  }

  function view () {
    final annsWg = Q("div");
    new Annotations(annsWg, anns, Some(i -> del(i)));

    editor.removeAll().add(selector());

    wg
      .removeAll()
      .add(Q("div")
        .add(Q("div")
          .klass("head")
          .html(_("Annotations")))
        .add(Q("table")
          .att("align", "center")
          .klass("frame3")
          .add(Q("tr")
            .add(Q("td")
              .add(editor)))
          .add(Q("tr")
            .add(Q("td")
              .add(Q("hr"))))
          .add(Q("tr")
            .add(Q("td")
              .add(Q("table").att("align", "right")
                .add(Q("tr")
                  .add(Q("td")
                    .klass("rlabel")
                    .add(Q("span")
                      .html(_("Cash:"))))
                  .add(Q("td")
                    .klass("number")
                    .text(Dec.toIso(cash, 2)))
                  .add(Q("td"))))))
          .add(Q("tr")
            .add(Q("td").klass("frame")
              .add(annsWg)))))
    ;
  }

  // Control -------------------------------------------------------------------

  function del (annId: Int) {
    final ann = Opt.get(It.from(anns).find(a -> a.id == annId));
    if (ann == null) {
      Ui.alert(_args(
        _("Annotation con id '%0' not found"), [Std.string(annId)]
      ));
      return;
    }
    if (ann.operation.st().ok) {
      Ui.alert(_("Intial stockage can not be deleted"));
      return;
    }
    if (Ui.confirm(_args(_("Delete %0?"), [ann.toJs().to()]))) {
      Cts.client.ssend([
        "module" => Js.ws("settings"),
        "source" => Js.ws("acc/editor"),
        "rq" => Js.ws("del"),
        "investorId" => Js.wi(investorId),
        "annId" => Js.wi(annId)
      ], rp -> {
        anns = rp["anns"].ra().map(e -> Annotation.fromJs(e));
        cash = rp["cash"].rf();
        view();
      });
    }
  }

  function sendAnn (
    type: String, date: String, s: String, i: Int, f: Float
  ) {
    final d = Dt.to(Opt.eget(Dt.fromIso(date)));
    final ann =
      type == "se" || type == "bu" ?
        [Js.wi(0), Js.ws(d), Js.ws(type), Js.ws(s), Js.wi(i), Js.wf(f)]
        : type == "in" || type == "wi" ?
          [Js.wi(0), Js.ws(d), Js.ws(type), Js.wf(f)]
          : [Js.wi(0), Js.ws(d), Js.ws(type), Js.wf(f), Js.ws(s)]
    ;
    Cts.client.ssend([
      "module" => Js.ws("settings"),
      "source" => Js.ws("acc/editor"),
      "rq" => Js.ws("new"),
      "investorId" => Js.wi(investorId),
      "ann" => Js.wa(ann)
    ], rp -> {
      anns = rp["anns"].ra().map(e -> Annotation.fromJs(e));
      cash = rp["cash"].rf();
      view();
    });
  }

  // Static --------------------------------------------------------------------

  /// Constructor
  ///   wg        : Container.
  ///   year      : Year to edit.
  ///   investorId: Investor to edit.
  public static function mk (wg: Domo, year: String, investorId: Int) {
    Cts.client.ssend([
      "module" => Js.ws("settings"),
      "source" => Js.ws("acc/editor"),
      "rq" => Js.ws("idata"),
      "year" => Js.ws(year),
      "investorId" => Js.wi(investorId)
    ], rp -> {
      final anns = rp["anns"].ra().map(e -> Annotation.fromJs(e));
      final cash = rp["cash"].rf();
      new Editor(wg, year, investorId, anns, cash);
    });
  }

  static function controlBase (id: String, value: String): String {
    if (value == "") {
      return _args(_("'%0' is empty"), [id]);
    }
    return "";
  };

  static function controlInt (id: String, value: String): String {
    var r = "";
    It.range(value.length).each(c -> {
      if (r == "" && (value.charAt(c) < "0" || value.charAt(c) > "9")) {
        r = _args(_("'%0' is not a positive integer"), [id]);
      }
    });
    return r;
  };

  static function controlDouble (
    lang: String, id: String, value: String
  ): String {
    if (
      (lang == "es" &&
        (
          Dec.fromIso(value) == None || Opt.get(Dec.fromIso(value)) <= 0 ||
          value.indexOf(".") != -1
        )
      ) ||
      (lang == "en" &&
        (
          Dec.fromIso(value) == None || Opt.get(Dec.fromEn(value)) <= 0 ||
          value.indexOf(",") != -1
        )
      )
    ) {
      return _args(_("'%0' is not a positive number"), [id]);
    }
    return "";
  };

  static function controlBuyPrice (
    lang: String, id: String, value: String
  ): String {
    if (
      (lang == "es" &&
        (
          Dec.fromIso(value) == None || Opt.get(Dec.fromIso(value)) <= 0 ||
          value.indexOf(".") != -1
        )
      ) ||
      (lang == "en" &&
        (
          Dec.fromIso(value) == None || Opt.get(Dec.fromEn(value)) <= 0 ||
          value.indexOf(",") != -1
        )
      )
    ) {
      return _args(_("'%0' is not a cero or positive number"), [id]);
    }
    return "";
  };

}
