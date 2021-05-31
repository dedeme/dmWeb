// Copyright 20-May-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package widgets;

import js.html.Audio;
import dm.Domo;
import dm.Ui;
import dm.Ui.Q;
import data.Pict;
import data.Song;
import I18n._;

/// Information widget.
class Info {
  public final wg = Q("div");
  final content = Q("div");
  final padding: Int;

  public function new (padding: Int, content: Domo) {
    this.padding = padding;
    this.content.add(content);

    view();
  }

  // VIEW

  function view () {
    wg
      .removeAll()
      .style(
          "z-index:3;" +
          "position:relative;" +
          "top: " + padding + "px;" +
          "left:15px;" +
          "opacity:0;" +
          "transition: opacity 2s linear;"
        )
      .add(content)
    ;
  }

  // CONTROL

  public function changeOpacity(): Void {
    final isHidden = wg.e.style.getPropertyValue("opacity") == "0";
    wg.setStyle("opacity", isHidden ? "1" : "0");
  }

  public function changeContent(content: Domo) {
    this.content.removeAll().add(content);
  }

  // STATIC

  /// Returns picture information widget.
  public static function pictureWg(group: String, pict: Pict): Domo {
    return Q("table")
      .klass("frame3")
      .add(Q("tr")
        .add(Q("td")
          .att("colspan", "3")
          .add(Ui.hrule(_("Picture")))))
      .add(Q("tr")
        .add(Q("td")
          .style("text-align:center")
          .text(_("Group")))
        .add(Q("td")
          .style("padding-left:5px")
          .text(_("Name")))
        .add(Q("td")
          .style("text-align:center")
          .text(_("Sights"))))
      .add(Q("tr")
        .add(Q("td")
          .klass("frame")
          .style("width:45px;white-space:nowrap;text-align:center")
          .text(group))
        .add(Q("td")
          .klass("frame")
          .style("max-width: 500px;white-space:nowrap;overflow: hidden;")
          .text(pict.id))
        .add(Q("td")
          .style("width:45px;white-space:nowrap;text-align:center")
          .klass("frame")
          .text(pict.sights + " / " + pict.level)))
    ;
  }

  /// Returns song information widget.
  public static function songWg(
    songGroup: String, song: Song, audio: Audio
  ): Domo {
    return Q("table")
      .klass("frame3")
      .add(Q("tr")
        .add(Q("td")
          .att("colspan", "3")
          .add(Ui.hrule(_("Song")))))
      .add(Q("tr")
        .add(Q("td")
          .style("text-align:center")
          .text(_("Group")))
        .add(Q("td")
          .style("padding-left:5px")
          .text(_("Name")))
        .add(Q("td")
          .style("text-align:center")
          .text(_("Sights"))))
      .add(Q("tr")
        .add(Q("td")
          .klass("frame")
          .style("width:45px;white-space:nowrap;text-align:center")
          .text(songGroup))
        .add(Q("td")
          .klass("frame")
          .style("max-width: 500px;white-space:nowrap;overflow: hidden;")
          .text(song.id))
        .add(Q("td")
          .style("width:45px;white-space:nowrap;text-align:center")
          .klass("frame")
          .text(song.sights + " / " + song.level)))
      .add(Q("tr")
        .add(Q("td")
          .att("colspan", "3")
          .style("text-align:center")
          .add(new Domo(audio))))
    ;
  }

  /// Returns dance information widget.
  /// 'time' is in seconds.
  public static function danceWg(
    songGroup: String, song: String, time: Int, audio: Audio
  ): Domo {
    return Q("table")
      .klass("frame3")
      .add(Q("tr")
        .add(Q("td")
          .att("colspan", "3")
          .add(Ui.hrule(_("Dance Song")))))
      .add(Q("tr")
        .add(Q("td")
          .style("text-align:center")
          .text(_("Group")))
        .add(Q("td")
          .style("padding-left:5px")
          .text(_("Name")))
        .add(Q("td")
          .style("text-align:center")
          .text(_("Time"))))
      .add(Q("tr")
        .add(Q("td")
          .klass("frame")
          .style("width:45px;white-space:nowrap;text-align:center")
          .text(songGroup))
        .add(Q("td")
          .klass("frame")
          .style("max-width: 500px;white-space:nowrap;overflow: hidden;")
          .text(song))
        .add(Q("td")
          .style("width:45px;white-space:nowrap;text-align:center")
          .klass("frame")
          .text(time + "'")))
      .add(Q("tr")
        .add(Q("td")
          .att("colspan", "3")
          .style("text-align:center")
          .add(new Domo(audio))))
    ;
  }

}
