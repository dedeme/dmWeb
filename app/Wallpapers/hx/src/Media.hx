// Copyright 24-May-2021 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

import js.Browser.window;
import dm.Client;
import dm.Domo;
import dm.Ui.Q;
import data.Pict;
import widgets.Info;
import I18n._;

class Media {
  // CONSTANTS

  /// Screen width.
  public static final screenWidth:Int = window.screen.width;
  /// Screen height.
  public static final screenHeight: Int = window.screen.height;
  /// Pictures width
  public static final picturesWidth: Int = 1920;
  /// Pictures height
  public static final picturesHeight: Int = 1080;
  /// height / width proportion
  public static final picturesProportion: Float = 0.5625;
  /// Time to access server in Pictures page (milliseconds)
	public static final picturesTime = 15000;
  /// Sound volume
  public static final volume = 0.5;
  /// Time to access server to update lapse in songs page (milliseconds)
	public static final songsTime = 5000;
  /// Time of fade out in dance (milliseconds)
  public static final fadeOutSongEnd = 12000.0; // 12''
  /// Time of fade out in dance (milliseconds)
  public static final fadeOutDanceTime = 300000.0; // 5'

  // FUNCTIONS

  // Calculate values to adapt pictures to screens.
  static function redimPicture(): {
    w: Int, h: Int, leftPadding: Int, topPadding: Int
  } {
    final prop =  screenHeight / screenWidth;
    if (screenWidth == picturesWidth && screenHeight == picturesWidth) {
      return {
          w: picturesWidth,
          h: picturesHeight,
          leftPadding: 0,
          topPadding: 0
        };
    } else if (prop < picturesProportion) {
      final w2 = Std.int(picturesWidth * screenHeight / picturesHeight);
      return {
          w: w2,
          h: screenHeight,
          leftPadding: Std.int((screenWidth - w2) / 2),
          topPadding: 0
        };
    } else {
      final h2 = Std.int(picturesHeight * screenWidth / picturesWidth);
      return {
          w: screenWidth,
          h: h2,
          leftPadding: 0,
          topPadding: Std.int((screenHeight - h2) / 2)
        };
    }
  }

  /// Returns visual elements
  public static function visuals(): {
    div: Domo, img: Domo, time: Domo
  } {
    final redim = redimPicture();

    final img = Q("img")
      .style(
        "width:" + redim.w +"px;" +
        "height:" + redim.h + "px;" +
        "padding-top:" + redim.topPadding + "px;" +
        "padding-left:" + redim.leftPadding + "px;" +
        "z-index:1;" +
        "transition: opacity 5s linear;"
      )
    ;

    final time = Q("div")
      .style(
        "z-index:2;" +
        "position:relative;" +
        "top:-250px;" +
        "left:0px;"
      )
    ;

    final div = Q("div")
      .style(
        "width:" + screenWidth +"px;" +
        "height:" + screenHeight + "px;" +
        "background-position: " +
          "top " + redim.topPadding + "px " +
          "right " + redim.leftPadding + "px;" +
        "background-repeat: no-repeat;" +
        "background-size:" + redim.w + "px " + redim.h + "px;"
      )
      .add(img)
      .add(time)
    ;

    return {
      div: div,
      img: img,
      time: time
    }
  }

  /// Change picture in display.
  ///   div: Container of 'img'.
  ///   img: Widget that show pictures.
  ///   info: 'img' data.
  ///   group: Group of the new picture to show.
  ///   picture: New picture to show.
  public static function changePict (
    div: Domo, img: Domo, info: Info,
    group: String, pict: Pict
  ): Void {
    final url = "img/fondosEscritorio/" + group + "/" + pict.id;
    div
      .setStyle(
        "background-image",
        "url('" + url + "')"
      );
    haxe.Timer.delay(() -> {
      img
        .setStyle("opacity", "0")
      ;
      haxe.Timer.delay(() -> {
        info.changeContent(Info.pictureWg(group, pict));
        img
          .att("src", url)
          .setStyle("opacity", "1")
        ;
      }, 8000);
    }, 2000);
  }

  /// Fades out audio
  public static function fadeOut(
    withSignal: Bool, audio: js.html.Audio, millis: Float
  ) {
    final fadeSec = millis / 1000;
    var vol = audio.volume;
    if (withSignal) vol /= 3;
    audio.volume = vol;
    final delta = vol / (fadeSec * 10);
    final tm2 = new haxe.Timer(100);
    tm2.run = () -> {
      vol -= delta;
      if (vol <= 0) {
        audio.volume = 0;
        tm2.stop();
        return;
      }
      audio.volume = vol;
    }
  }

}
