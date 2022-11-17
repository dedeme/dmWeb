// Copyright 16-Jun-2020 ºDeme
// GNU General Public License - V3 <http://www.gnu.org/licenses/>

package dm;

import dm.Domo;
import dm.Ui.Q;

/// Clock widget.
class Clock {
  /// 'true' if Clock is a chronometer.
  public final isChron: Bool;
  /// Default 120
  public var width: Int = 120;
  /// Default 120
  public var height: Int = 120;
  /// Background color. Default "#ffffff".
  public var bg: String = "#ffffff";
  /// Numbers color. Default "#000033".
  public var number: String = "#000033";
  /// Axis color. Default "#446688".
  public var axis: String = "#446688";
  /// Numbers color. Default "#446688".
  public var hhand: String = "#446688";
  /// Numbers color. Default "#446688".
  public var mhand: String = "#446688";
  /// Numbers color. Default "#000033".
  public var shand: String = "#000033";
  /// Clock widget
  public var wg(get, never): Domo;
  function get_wg () return mk();

  final start: Null<Date>;

  public function new (isChron = false) {
    this.isChron = isChron;
    start = isChron ? Date.now() : null;
  }

  /// Returns clock widget.
  function mk (): Domo {
    final cv = Q("canvas")
      .att("width", this.width)
      .att("height", this.height)
    ;
    final el: js.html.CanvasElement = cast(cv.e);
    final ctx: js.html.CanvasRenderingContext2D = el.getContext("2d");
    var radius = el.height / 2;
    ctx.translate(radius, radius);
    radius = radius * 0.90;
    this.drawBack(ctx, radius);
    this.paint(ctx, radius);

    final tm = new haxe.Timer(1000);
    tm.run = () -> this.paint(ctx, radius);

    return cv;
  }

  function paint (ctx: js.html.CanvasRenderingContext2D, radius: Float): Void {
    this.drawBorder(ctx, radius);
    this.drawNumbers(ctx, radius);
    this.drawTime(ctx, radius);
    this.drawAxis(ctx, radius);
  }

  function drawBack (
    ctx: js.html.CanvasRenderingContext2D, radius: Float
  ): Void {
    ctx.beginPath();
    ctx.arc(0, 0, radius, 0, 2 * Math.PI);
    ctx.fillStyle = this.bg;
    ctx.fill();
    final grad = ctx.createRadialGradient(
      0, 0, radius * 0.95, 0, 0, radius * 1.05
    );
    grad.addColorStop(0, "#333");
    grad.addColorStop(0.5, "white");
    grad.addColorStop(1, "#333");
    ctx.strokeStyle = grad;
    ctx.lineWidth = radius * 0.1;
    ctx.stroke();
  }

  function drawBorder (
    ctx: js.html.CanvasRenderingContext2D, radius: Float
  ): Void {
    ctx.beginPath();
    ctx.arc(0, 0, radius * 0.93, 0, 2 * Math.PI);
    ctx.fillStyle = this.bg;
    ctx.fill();
  }

  function drawNumbers (
    ctx: js.html.CanvasRenderingContext2D, radius: Float
  ): Void {
    ctx.fillStyle = this.number;
    ctx.font = radius * 0.16 + "px sans-serif";
    ctx.textBaseline = "middle";
    ctx.textAlign = "center";
    for (num in 1...13) {
      final ang = num * Math.PI / 6;
      ctx.rotate(ang);
      ctx.translate(0, -radius * 0.82);
      ctx.rotate(-ang);
      ctx.fillText(Std.string(num), 0, 0);
      ctx.rotate(ang);
      ctx.translate(0, radius * 0.82);
      ctx.rotate(-ang);
    }
  }

  function drawTime (
    ctx: js.html.CanvasRenderingContext2D, radius: Float
  ): Void {
    var now = Date.now();
    if (isChron) {
      now = Date.fromTime(now.getTime() - start.getTime() - 3600000);
    }
    var hours = now.getHours();
    var minute: Float = now.getMinutes();
    var second: Float = now.getSeconds();
    //hour
    hours = hours % 12;
    final hour: Float = (hours * Math.PI / 6) +
      (minute * Math.PI / (6 * 60)) +
      (second * Math.PI / (360 * 60));
    this.drawHand(ctx, hour, radius * 0.5, radius * 0.07, this.hhand);
    //minute
    minute = (minute * Math.PI / 30) + (second * Math.PI / (30 * 60));
    this.drawHand(ctx, minute, radius * 0.8, radius * 0.07, this.mhand);
    // second
    second = second * Math.PI / 30;
    this.drawHand(ctx, second, radius * 0.9, radius * 0.02, this.shand);
  }

  function drawHand (
    ctx: js.html.CanvasRenderingContext2D,
    pos: Float, len: Float, width: Float, color: String
  ): Void {
    ctx.beginPath();
    ctx.lineWidth = width;
    ctx.lineCap = "round";
    ctx.moveTo(0, 0);
    ctx.rotate(pos);
    ctx.lineTo(0, -len);
    ctx.strokeStyle = color;
    ctx.stroke();
    ctx.rotate(-pos);
  }

  function drawAxis (
    ctx: js.html.CanvasRenderingContext2D, radius: Float
  ): Void {
    ctx.beginPath();
    ctx.arc(0, 0, radius * 0.1, 0, 2 * Math.PI);
    ctx.fillStyle = this.axis;
    ctx.fill();
  }

}
