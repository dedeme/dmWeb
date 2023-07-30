import * as iter from '../_js/iter.js';import * as str from '../_js/str.js';import * as bytes from '../_js/bytes.js';import * as cryp from '../_js/cryp.js';import * as dic from '../_js/dic.js';import * as timer from '../_js/timer.js';import * as js from '../_js/js.js';import * as storage from '../_js/storage.js';import * as sys from '../_js/sys.js';import * as math from '../_js/math.js';import * as domo from '../_js/domo.js';import * as ui from '../_js/ui.js';import * as arr from '../_js/arr.js';import * as time from '../_js/time.js';import * as client from '../_js/client.js';import * as b64 from '../_js/b64.js';




const Q =sys.$checkNull( ui.q);



export  function mk()  {sys.$params(arguments.length, 0);  return mk2(false);};



export  function mkChron()  {sys.$params(arguments.length, 0);  return mk2(true);};


 function mk2(isChron)  {sys.$params(arguments.length, 1);  return {
  isChron:isChron,
  start: time.now(),
  width: 120,
  height: 120,
  bg: "#ffffff",
  number: "#000033",
  axis: "#446688",
  hhand: "#446688",
  mhand: "#446688",
  shand: "#000033"
};};



export  function width(C)  {sys.$params(arguments.length, 1);  return C.width;};



export  function setWidth(C, v)  {sys.$params(arguments.length, 2); C.width =sys.$checkExists(C.width,sys.$checkNull( v));};



export  function height(C)  {sys.$params(arguments.length, 1);  return C.height;};



export  function setHeight(C, v)  {sys.$params(arguments.length, 2); C.height =sys.$checkExists(C.height,sys.$checkNull( v));};



export  function bg(C)  {sys.$params(arguments.length, 1);  return C.bg;};



export  function setBg(C, c)  {sys.$params(arguments.length, 2); C.bg =sys.$checkExists(C.bg,sys.$checkNull( c));};



export  function number(C)  {sys.$params(arguments.length, 1);  return C.number;};



export  function setNumber(C, c)  {sys.$params(arguments.length, 2); C.number =sys.$checkExists(C.number,sys.$checkNull( c));};



export  function axis(C)  {sys.$params(arguments.length, 1);  return C.axis;};



export  function setAxis(C, c)  {sys.$params(arguments.length, 2); C.axis =sys.$checkExists(C.axis,sys.$checkNull( c));};



export  function hhand(C)  {sys.$params(arguments.length, 1);  return C.hhand;};



export  function setHhand(C, c)  {sys.$params(arguments.length, 2); C.hhand =sys.$checkExists(C.hhand,sys.$checkNull( c));};



export  function mhand(C)  {sys.$params(arguments.length, 1);  return C.mhand;};



export  function setMhand(C, c)  {sys.$params(arguments.length, 2); C.mhand =sys.$checkExists(C.mhand,sys.$checkNull( c));};



export  function shand(C)  {sys.$params(arguments.length, 1);  return C.shand;};



export  function setShand(C, c)  {sys.$params(arguments.length, 2); C.shand =sys.$checkExists(C.shand,sys.$checkNull( c));};



export  function wg(C)  {sys.$params(arguments.length, 1);
  const cv =sys.$checkNull( Q("canvas")
    .att("width", C.width)
    .att("height", C.height))
  ;
  const el =sys.$checkNull( cv.e);
  const ctx =sys.$checkNull( el.getContext("2d"));
  const radius0 =sys.$checkNull( el.height / 2);
  ctx.translate(radius0, radius0);
  const radius =sys.$checkNull( radius0 * 0.90);

  
   function drawBack()  {sys.$params(arguments.length, 0);
    ctx.beginPath();
    ctx.arc(0, 0, radius, 0, 2 * Math.PI);
    ctx.fillStyle =sys.$checkExists(ctx.fillStyle,sys.$checkNull( C.bg));
    ctx.fill();
    const grad =sys.$checkNull( ctx.createRadialGradient(
      0, 0, radius * 0.95, 0, 0, radius * 1.05
    ));
    grad.addColorStop(0, "#333");
    grad.addColorStop(0.5, "white");
    grad.addColorStop(1, "#333");
    ctx.strokeStyle =sys.$checkExists(ctx.strokeStyle,sys.$checkNull( grad));
    ctx.lineWidth =sys.$checkExists(ctx.lineWidth,sys.$checkNull( radius * 0.1));
    ctx.stroke();
  };

  
   function drawBorder()  {sys.$params(arguments.length, 0);
    ctx.beginPath();
    ctx.arc(0, 0, radius * 0.93, 0, 2 * Math.PI);
    ctx.fillStyle =sys.$checkExists(ctx.fillStyle,sys.$checkNull( C.bg));
    ctx.fill();
  };

  
   function drawNumbers()  {sys.$params(arguments.length, 0);
    ctx.fillStyle =sys.$checkExists(ctx.fillStyle,sys.$checkNull( C.number));
    ctx.font =sys.$checkExists(ctx.font,sys.$checkNull( radius * 0.16 + "px sans-serif"));
    ctx.textBaseline =sys.$checkExists(ctx.textBaseline,sys.$checkNull( "middle"));
    ctx.textAlign =sys.$checkExists(ctx.textAlign,sys.$checkNull( "center"));
    for (let num  of sys.$forObject( iter.$range(1,13))) {
      const ang =sys.$checkNull( num * Math.PI / 6);
      ctx.rotate(ang);
      ctx.translate(0,  -radius * 0.82);
      ctx.rotate( -ang);
      ctx.fillText(math.toStr(num), 0, 0);
      ctx.rotate(ang);
      ctx.translate(0, radius * 0.82);
      ctx.rotate( -ang);
    }
  };

  
   function drawHand(pos, len, width, color)  {sys.$params(arguments.length, 4);
    ctx.beginPath();
    ctx.lineWidth =sys.$checkExists(ctx.lineWidth,sys.$checkNull( width));
    ctx.lineCap =sys.$checkExists(ctx.lineCap,sys.$checkNull( "round"));
    ctx.moveTo(0, 0);
    ctx.rotate(pos);
    ctx.lineTo(0,  -len);
    ctx.strokeStyle =sys.$checkExists(ctx.strokeStyle,sys.$checkNull( color));
    ctx.stroke();
    ctx.rotate( -pos);
  };

  
   function drawTime()  {sys.$params(arguments.length, 0);
    const now =sys.$checkNull(sys.asBool( C.isChron)
      ? time.now() - C.start - 3600000
      : time.now())
    ;

    const hour0 =sys.$checkNull( time.hour(now) % 12);
    const minute0 =sys.$checkNull( time.minute(now));
    const second0 =sys.$checkNull( time.second(now));
    
    const hour =sys.$checkNull( (hour0 * Math.PI / 6) +
      (minute0 * Math.PI / (6 * 60)) +
      (second0 * Math.PI / (360 * 60)));
    drawHand(hour, radius * 0.5, radius * 0.07, C.hhand);
    
    const minute =sys.$checkNull( (minute0 * Math.PI / 30) + (second0 * Math.PI / (30 * 60)));
    drawHand(minute, radius * 0.8, radius * 0.07, C.mhand);
    
    const second =sys.$checkNull( second0 * Math.PI / 30);
    drawHand(second, radius * 0.9, radius * 0.02, C.shand);
  };

  
   function drawAxis()  {sys.$params(arguments.length, 0);
    ctx.beginPath();
    ctx.arc(0, 0, radius * 0.1, 0, 2 * Math.PI);
    ctx.fillStyle =sys.$checkExists(ctx.fillStyle,sys.$checkNull( C.axis));
    ctx.fill();
  };

  
   function paint()  {sys.$params(arguments.length, 0);
    drawBorder();
    drawNumbers();
    drawTime();
    drawAxis();
  };

  drawBack();
  paint();
  timer.run(timer.mk(1000), paint);

   return cv;
};
