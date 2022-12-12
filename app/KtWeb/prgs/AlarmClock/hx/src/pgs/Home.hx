// Copyright 25-Nov-2022 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs;

using StringTools;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import I18n._;
import data.Alarm;

/// Home page.
class Home {
  final wg: Domo;
  final alarms:  Array<Alarm>;

  final botonSpan = Q("span");

  function new (wg: Domo, alarms: Array<Alarm>) {
    this.wg = wg;
    this.alarms = alarms;
  }

  // View ----------------------------------------------------------------------

  public function show (): Void {
    final entry = Ui.field("_accept")
      .att("id", "autofocus")
      .style("width:50px")
    ;
    final accept = Q("button")
      .att("id", "_accept")
      .text(_("Add"))
      .on(CLICK, () -> add(entry.getValue()))
    ;

    final trs = alarms.map(a -> Q("tr")
      .add(Q("td")
        .klass("frame")
        .style("text-align: right")
        .text(a.timeToStr()))
      .add(Q("td")
        .style("Text-align: left")
        .add(Ui.link(() -> del(a))
          .add(Ui.img("delete"))))
    );

    wg
      .removeAll()
      .add(Q("table")
        .att("align", "center")
        .add(Q("tr")
          .add(Q("td")
            .att("colspan", 2)
            .add(Q("div")
              .klass("head")
              .text(_("New Alarm")))))
        .add(Q("tr")
          .add(Q("td")
            .add(entry))
          .add(Q("td")
            .add(botonSpan
              .add(accept))))
        .add(Q("tr")
          .add(Q("td")
            .att("colspan", 2)
            .add(Q("hr"))))
        .add(Q("tr")
          .add(Q("td")
            .att("colspan", 2)
            .add(Q("div")
              .klass("head")
              .text(_("Programmed Alarms")))))
        .adds(alarms.length == 0
            ? [Q("tr")
                .add(Q("td")
                  .klass("frame")
                  .att("colspan", 2)
                  .style("text-align:center")
                  .text(_("Without Alarms")))
              ]
            : trs
          ))
    ;
    entry.e.focus();
  }

  // Control -------------------------------------------------------------------

  function add (entry: String): Void {
    function badFormat (): String {
      final or = _("or");
      return _("Bad Format. Use") + ":\n"+
        "  HH,MM " + or + " HH.MM " + or + " HH:MM"
      ;
    }

    final sep = entry.contains(",")
      ? ","
      : entry.contains(".")
        ? "."
        : entry.contains(":")
          ? ":"
          : "";
    if (sep == "") {
      Ui.alert(_("Separator is missing"));
      return;
    }

    final parts = entry.split(sep);
    if (parts.length != 2) {
      Ui.alert(badFormat());
      return;
    }

    final h = Std.parseInt(parts[0]);
    var ms = parts[1];
    if (ms.length == 1) {
      ms = "0" + ms;
    }
    final m = Std.parseInt(ms);
    if (h == null || m == null) {
      Ui.alert(badFormat());
      return;
    }
    if (h < 0 || h > 23) {
      Ui.alert(_("Hour out of range"));
      return;
    }
    if (m < 0 || m > 59) {
      Ui.alert(_("Minutes out of range"));
      return;
    }

    final tm = Date.now();
    final hnow = tm.getHours();
    final mnow = tm.getMinutes();

    final dayAlarm = h > hnow || (hnow == h && m > mnow)
      ? tm
      : dm.Dt.add(tm, 1)
    ;

    botonSpan.removeAll().add(Ui.img("wait.gif"));
    Global.client.ssend([
      "prg" => Js.ws("AlarmClock"),
      "source" => Js.ws("Home"),
      "rq" => Js.ws("add"),
      "key" => Js.ws(dm.Cryp.genK(6) + ":" + Std.string(tm.getTime())),
      "tm" => Js.wf(new Date(
          dayAlarm.getFullYear(), dayAlarm.getMonth(), dayAlarm.getDate(),
          h, m, 0
        ).getTime())
    ], rp -> {
      final isDup = rp["isDup"].rb();
      if (isDup) {
        Ui.alert(_("Duplicated alarm"));
      }
      mk(wg);
    });
  }

  function del (a : Alarm): Void {
    if (Ui.confirm(_("Remove the alarm") + " '" + a.timeToStr() + "'")) {
      Global.client.ssend([
        "prg" => Js.ws("AlarmClock"),
        "source" => Js.ws("Home"),
        "rq" => Js.ws("del"),
        "alarm" => a.toJs()
      ], rp -> {
        mk(wg);
      });
    }
  }

  // Static --------------------------------------------------------------------

  public static function mk (wg: Domo): Void {
    Global.client.send([
      "prg" => Js.ws("AlarmClock"),
      "source" => Js.ws("Home"),
      "rq" => Js.ws("idata")
    ], rp -> {
      final alarms = rp["alarms"].ra().map(Alarm.fromJs);
      new Home(wg, alarms).show();
    });
  }
}
