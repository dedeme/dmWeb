// Copyright 20-Jan-2023 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package pgs;

import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import dm.Js;
import dm.Opt;
import dm.B64;
import data.Tpath;
import data.JsData;
import data.EditorRp;
import data.FieldChange;
import data.Type;
import wgs.PathWg;
import wgs.NullEditor;
import wgs.BooleanEditor;
import wgs.NumberEditor;
import wgs.StringEditor;
import wgs.ArrayEditor;
import wgs.MapEditor;
import I18n._;
import I18n._args;

/// Editor page.
class Editor {
  final wg: Domo;
  final tpath: Tpath;
  final jsData: JsData;

  function new (wg: Domo, tpath: Tpath, jsData: JsData) {
    this.wg = wg;
    this.tpath = tpath;
    this.jsData = jsData;
  }

  // View ----------------------------------------------------------------------

  public function show (): Void {
    final pathDiv = Q("div");
    final body = Q("div");

    wg
      .removeAll()
      .add(Q("table")
        .klass("main")
        .add(Q("tr")
          .add(Q("td")
            .style("width:5px;text-align:left;vertical-align:top")
            .add(pathDiv))
          .add(Q("td")
            .style("vertical-align:top")
            .add(body))))
    ;

    new PathWg(pathDiv, tpath, reload).show();

    if (jsData.type == Type.NULL)
      new NullEditor(body, jsData, set).show();
    else if (jsData.type == Type.BOOLEAN)
      new BooleanEditor(body, jsData, set).show();
    else if (jsData.type == Type.NUMBER)
      new NumberEditor(body, jsData, set).show();
    else if (jsData.type == Type.STRING)
      new StringEditor(body, jsData, set).show();
    else if (jsData.type == Type.ARRAY)
      new ArrayEditor(body, tpath, jsData, set).show();
    else if (jsData.type == Type.MAP)
      new MapEditor(body, tpath, jsData, set).show();
    else
      throw ("Unknown type " + jsData.type);
  }

  // Control -------------------------------------------------------------------

  function set (change: FieldChange, newTpath: Option<Tpath>): Void {
    Global.client.send([
      "prg" => Js.ws("JstbEditor"),
      "source" => Js.ws("Editor"),
      "rq" => Js.ws("set"),
      "tpath" => tpath.toJs(),
      "change" => change.toJs()
    ], rp -> {
      final response = EditorRp.fromJs(rp["response"]);

      switch (response.error) {
        case EditorRp.NO_TABLE: {
          new MsgPg(
            wg,
            _args(
              _("<p>Table<br>%0<br>not found.</p>"), [tpath.table.fpath]
            ),
            true
          ).show();
          return;
        }
        case EditorRp.NO_JSON: {
          new MsgPg(
            wg,
            _args(
              _("<p>File<br>%0<br>does not contain a JSON table.</p>"),
              [tpath.table.fpath]
            ),
            true
          ).show();
          return;
        }
        case EditorRp.MODIFIED: {
          Ui.alert("Table no synchronized.\nNo change has been made");
          js.Browser.location.assign("?");
        }
        default:
          reload(switch(newTpath) { case None: tpath; case Some(tp): tp; });
      }

    });
  }

  // Static --------------------------------------------------------------------

  public static function mk (wg: Domo, tpath: Tpath): Void {
    final waitDiv = Q("div")
      .add(Ui.img("wait.gif"))
    ;
    final body = Q("div");
    wg
      .removeAll()
      .add(Q("table")
        .klass("main")
        .add(Q("tr")
          .add(Q("td")
            .style("width:5px")
            .add(waitDiv))
          .add(Q("td")
            .klass("frame")
            .style("text-aling:left;width:5px")
            .text(tpath.table.base()))
          .add(Q("td")
            .style(
                "color:#607080;text-aling:left;" +
                "width:5px;white-space:nowrap"
              )
            .text(tpath.table.shortPath()))
          .add(Q("td")
            .style("text-align:right")
            .add(Ui.link(() -> js.Browser.location.assign("?"))
              .klass("link")
              .text(_("List")))))
        .add(Q("tr").add(Q("td").att("colspan", 4).add(Q("hr"))))
        .add(Q("tr").add(Q("td").att("colspan", 4).add(body))))
    ;
    Global.client.send([
      "prg" => Js.ws("JstbEditor"),
      "source" => Js.ws("Editor"),
      "rq" => Js.ws("idata"),
      "tpath" => tpath.toJs()
    ], rp -> {
      waitDiv.removeAll().add(Ui.led("#104080"));

      final response = EditorRp.fromJs(rp["response"]);
      switch (response.error) {
        case EditorRp.NO_TABLE: {
          new MsgPg(
            wg,
            _args(
              _("<p>Table<br>%0<br>not found.</p>"), [tpath.table.fpath]
            ),
            true
          ).show();
          return;
        }
        case EditorRp.NO_JSON: {
          new MsgPg(
            wg,
            _args(
              _("<p>File<br>%0<br>does not contain a JSON table.</p>"),
              [tpath.table.fpath]
            ),
            true
          ).show();
          return;
        }
        case EditorRp.MODIFIED: {
          Ui.alert("Table no synchronized.");
          js.Browser.location.assign("?");
        }
        default:
          new Editor(body, tpath, Opt.get(response.jdata)).show();
      }
    });
  }

  public static function reload (tpath: Tpath): Void {
    var hash = B64.encode(tpath.toJs().to());
    while (hash != "" && hash.charAt(hash.length - 1) == "=")
      hash = hash.substring(0, hash.length - 1);
    js.Browser.location.assign("?" + hash);
  }
}
