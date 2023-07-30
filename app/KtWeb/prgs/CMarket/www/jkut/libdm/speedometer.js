import * as iter from '../_js/iter.js';import * as str from '../_js/str.js';import * as bytes from '../_js/bytes.js';import * as cryp from '../_js/cryp.js';import * as dic from '../_js/dic.js';import * as timer from '../_js/timer.js';import * as js from '../_js/js.js';import * as storage from '../_js/storage.js';import * as sys from '../_js/sys.js';import * as math from '../_js/math.js';import * as domo from '../_js/domo.js';import * as ui from '../_js/ui.js';import * as arr from '../_js/arr.js';import * as time from '../_js/time.js';import * as client from '../_js/client.js';import * as b64 from '../_js/b64.js';




const Q =sys.$checkNull( ui.q);









export  function mk(value, wgRatio, borderOp, backOp)  {sys.$params(arguments.length, 4);
  const ratio =sys.$checkNull(sys.asBool( wgRatio < 0) ? 0 : wgRatio);
  const w =sys.$checkNull( math.toInt(300 * ratio));
  const h =sys.$checkNull( math.toInt(170 * ratio));

  const styleV =sys.$checkNull( [""]);
  styleV[0] +=sys.$checkExists(styleV[0],sys.$checkNull(sys.asBool( borderOp) ? "border:1px solid " + borderOp[0] + ";" : ""));
  styleV[0] +=sys.$checkExists(styleV[0],sys.$checkNull(sys.asBool( backOp) ? "background:" + backOp[0] + ";" : ""));

  const wg =sys.$checkNull( Q("canvas")
    .att("width", w)
    .att("height", h)
    .style(styleV[0]))
  ;

  const ctx =sys.$checkNull( wg.e.getContext("2d"));

  
   function mkPt(x, y)  {sys.$params(arguments.length, 2);
    const R =sys.$checkNull( {x:x, y:y});
    dic.put(R, "rotate", function(angle)  {sys.$params(arguments.length, 1);
      const cos =sys.$checkNull( Math.cos(angle));
      const sin =sys.$checkNull( Math.sin(angle));
       return mkPt( 
        -R.x * cos + R.y * sin,
        R.y * cos + R.x * sin
      );
    });
     return R;
  };


  
   function mkDial()  {sys.$params(arguments.length, 0);
    
     function grad(P1, P2, c1, c2)  {sys.$params(arguments.length, 4);
      const grd =sys.$checkNull( ctx.createLinearGradient(
        math.toInt(P1.x), math.toInt(P1.y),
        math.toInt(P2.x), math.toInt(P2.y)
      ));
      grd.addColorStop(0, c1);
      grd.addColorStop(1, c2);
       return grd;
    };

    ctx.lineWidth =sys.$checkExists(ctx.lineWidth,sys.$checkNull( math.toInt(50 * ratio)));
    const radius =sys.$checkNull( math.toInt(120 * ratio));

    
     function arc(start, end)  {sys.$params(arguments.length, 2);
      ctx.beginPath();
      ctx.arc(
        math.toInt(150 * ratio), math.toInt(150 * ratio),
        radius,
        start, end,
        false
      );
      ctx.stroke();
    };

    ctx.strokeStyle =sys.$checkExists(ctx.strokeStyle,sys.$checkNull( grad(
      mkPt(28 * ratio, 146 * ratio),
      mkPt(65 * ratio, 64 * ratio),
      "#40a040", "#4040a0"
    )));
    arc( -Math.PI * 1,  -Math.PI * 0.65);

    ctx.strokeStyle =sys.$checkExists(ctx.strokeStyle,sys.$checkNull( grad(
      mkPt(65 * ratio, 64 * ratio),
      mkPt(149 * ratio, 29 * ratio),
      "#4040a0", "#a040f0"
    )));
    arc( -Math.PI * 0.75,  -Math.PI * 0.5);


    ctx.strokeStyle =sys.$checkExists(ctx.strokeStyle,sys.$checkNull( grad(
      mkPt(149 * ratio, 29 * ratio),
      mkPt(237 * ratio, 64 * ratio),
      "#a040f0", "#a040a0"
    )));
    arc( -Math.PI * 0.50,  -Math.PI * 0.25);

    ctx.strokeStyle =sys.$checkExists(ctx.strokeStyle,sys.$checkNull( grad(
      mkPt(237 * ratio, 64 * ratio),
      mkPt(273 * ratio, 146 * ratio),
      "#a040a0", "#a04040"
    )));
    arc( -Math.PI * 0.255,  -Math.PI * 0);
  };

  
   function mkNeedle()  {sys.$params(arguments.length, 0);
    const angle =sys.$checkNull( Math.PI * value);

    const dx =sys.$checkNull( math.toInt(150 * ratio));
    const dy =sys.$checkNull( math.toInt(150 * ratio));
    
     function getX(P)  {sys.$params(arguments.length, 1);  return P.x + dx;};
    
     function getY(P)  {sys.$params(arguments.length, 1);  return dy - P.y;};

    const P1 =sys.$checkNull( mkPt(130 * ratio, 0).rotate(angle));
    const P2 =sys.$checkNull( mkPt(0,  -16 * ratio).rotate(angle));
    const P3 =sys.$checkNull( mkPt(0, 16 * ratio).rotate(angle));

    ctx.lineJoin =sys.$checkExists(ctx.lineJoin,sys.$checkNull( "round"));
    ctx.lineWidth =sys.$checkExists(ctx.lineWidth,sys.$checkNull( 4 * ratio));
    ctx.strokeStyle =sys.$checkExists(ctx.strokeStyle,sys.$checkNull(sys.asBool( backOp) ? backOp[0] : "black"));
    ctx.fillStyle =sys.$checkExists(ctx.fillStyle,sys.$checkNull( "black"));

    ctx.beginPath();
    ctx.moveTo(getX(P1), getY(P1));
    ctx.lineTo(getX(P2), getY(P2));
    ctx.lineTo(getX(P3), getY(P3));
    ctx.closePath();
    ctx.stroke();
    ctx.fill();

    ctx.lineJoin =sys.$checkExists(ctx.lineJoin,sys.$checkNull( "miter"));
  };

  
   function mkArc()  {sys.$params(arguments.length, 0);
    ctx.lineWidth =sys.$checkExists(ctx.lineWidth,sys.$checkNull( math.toInt(6 * ratio)));
    ctx.strokeStyle =sys.$checkExists(ctx.strokeStyle,sys.$checkNull( "#406080"));
    ctx.fillStyle   =sys.$checkExists(ctx.fillStyle,sys.$checkNull( "black"));

    ctx.beginPath();
    ctx.arc(math.toInt(150 * ratio), math.toInt(150 * ratio),
    math.toInt(40 * ratio),
    0, Math.PI,
    true);
    ctx.lineTo(math.toInt(110 * ratio), math.toInt(165 * ratio));
    ctx.lineTo(math.toInt(190 * ratio), math.toInt(165 * ratio));
    ctx.closePath();
    ctx.fill();
    ctx.stroke();

    ctx.beginPath();
    ctx.moveTo(math.toInt(2 * ratio), math.toInt(165 * ratio));
    ctx.lineTo(math.toInt(298 * ratio), math.toInt(165 * ratio));
    ctx.stroke();
  };

  mkDial();
  mkNeedle();
  mkArc();

   return wg;
};
