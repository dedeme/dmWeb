// Copyright 10-Sep-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs.fleas.ftests;

import haxe.Timer;
import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.Opt;
import data.Cts;
import data.flea.Fmodel;
import data.flea.Flog;
import data.LogRow;
import wgs.Msg;
import I18n._;

/// Selection test page.
class Selection {
  var wg: Domo;
  var model: Fmodel;
  var log: Option<Flog>;
  var timer: Option<Timer>;
  final textArea = Q("textarea")
    .att("spellcheck", false)
    .att("readOnly", true)
    .att("rows", 25)
    .att("cols", 120)
  ;

  function new(wg: Domo, model:Fmodel, log: Flog) {
    this.wg = wg;
    this.model = model;
    this.log = Some(log);

    timer = None;
    view();
  }

  // View ----------------------------------------------------------------------

  function view () {
    wg
      .removeAll()
      .add(Q("table")
        .att("align", "center")
        .add(Q("tr")
          .add(Q("td")
            .add(Ui.link(e -> start())
              .klass("link")
              .text(_("Start")))
            .add(Q("span")
              .html("&nbsp;&nbsp;&nbsp;&nbsp;"))
            .add(Ui.link(e -> stop())
              .klass("link")
              .text(_("Stop")))))
        .add(Q("tr")
          .add(Q("td")
            .add(textArea))))
    ;
  }

  // Control -------------------------------------------------------------------

  function start (): Void {
    function loop () {
      final lid = switch(log) {
        case None: "";
        case Some(l): l.id;
      }

      Cts.client.send([
        "module" => Js.ws("fleas"),
        "source" => Js.ws("ftests/selection"),
        "rq" => Js.ws("continue"),
        "logId" => Js.ws(lid)
      ], rp -> {
        final logJs = rp["log"];
        if (logJs.isNull()) {
          log = None;
          final t = Opt.get(timer);
          if (t != null) t.stop();
          timer = None;
          textArea.value(
            "End process.\n" + cast(textArea.getValue(), String)
          );
        } else {
          final l = logJs.ra();
          textArea.value(
            l.map(e -> LogRow.fromJs(e).format(115)).join("\n")
          );
        }
      });
    }

    switch (log) {
      case None:
        Ui.alert(_("Play ground stoped.\nRestart doing click in 'Selection'."));
      case Some(l):
        Cts.client.send([
          "module" => Js.ws("fleas"),
          "source" => Js.ws("ftests/selection"),
          "rq" => Js.ws("start"),
          "modelId" => Js.ws(model.id),
          "logId" => Js.ws(l.id)
        ], rp -> {
          if (!rp["ok"].rb()) {
            Msg.error(Cts.failMsg);
          } else {
            loop();
            final tm = new Timer(2000);
            tm.run = loop;
            timer = Some(tm);
          }
        });
    }
  }

  function stop () {
    final l = Opt.get(log);
    if (timer == None || l == null) return;

    if (Ui.confirm(_("Stop process?"))) {
      Cts.client.send([
        "module" => Js.ws("fleas"),
        "source" => Js.ws("ftests/selection"),
        "rq" => Js.ws("stop"),
        "logId" => Js.ws(l.id)
      ], rp -> {
        log = None;
      });
    }
  }

  // Static --------------------------------------------------------------------

  /// Constructor.
  ///   wg   : Container.
  ///   model: Model to test.
  public static function mk (wg: Domo, model: Fmodel) {
    Cts.client.send([
      "module" => Js.ws("fleas"),
      "source" => Js.ws("ftests/selection"),
      "rq" => Js.ws("logId")
    ], rp -> {
      final id = rp["logId"].rs();
      new Selection(wg, model, new Flog(id, []));
    });
  }

}
