import * as iter from '../_js/iter.js';import * as str from '../_js/str.js';import * as bytes from '../_js/bytes.js';import * as cryp from '../_js/cryp.js';import * as dic from '../_js/dic.js';import * as timer from '../_js/timer.js';import * as js from '../_js/js.js';import * as storage from '../_js/storage.js';import * as sys from '../_js/sys.js';import * as math from '../_js/math.js';import * as domo from '../_js/domo.js';import * as ui from '../_js/ui.js';import * as arr from '../_js/arr.js';import * as time from '../_js/time.js';import * as client from '../_js/client.js';import * as b64 from '../_js/b64.js';












































































const Q =sys.$checkNull( ui.q);





export  function mkPadding(top, right, bottom, left)  {sys.$params(arguments.length, 4);
   return {top:top, right:right, bottom:bottom, left:left};};


export  function paddingCopy(P)  {sys.$params(arguments.length, 1);  return mkPadding(P.top, P.right, P.bottom, P.left);};


export  function paddingToJs(P)  {sys.$params(arguments.length, 1);  return [P.top, P.right, P.bottom, P.left];};


export  function paddingFromJs(A)  {sys.$params(arguments.length, 1);  return mkPadding(A[0], A[1], A[2], A[3]);};


export  function mkPaddingExample()  {sys.$params(arguments.length, 0);  return mkPadding(8, 10, 20, 60);};





export  function mkLine(width, color, dotted)  {sys.$params(arguments.length, 3);  return {width:width, color:color, dotted:dotted};};


export  function lineCopy(L)  {sys.$params(arguments.length, 1);  return mkLine(L.width, L.color, L.dotted);};


export  function lineToJs(L)  {sys.$params(arguments.length, 1);  return [L.width, L.color, L.dotted];};


export  function lineFromJs(A)  {sys.$params(arguments.length, 1);  return mkLine(A[0], A[1], A[2]);};


export  function mkLineExample()  {sys.$params(arguments.length, 0);  return mkLine(1, "#002040", false);};





export  function mkAreaAtts(Border, background)  {sys.$params(arguments.length, 2);  return {Border:Border, background:background};};


export  function areaAttsCopy(A)  {sys.$params(arguments.length, 1);  return mkAreaAtts(lineCopy(A.Border), A.background);};


export  function areaAttsToJs(A)  {sys.$params(arguments.length, 1);  return [lineToJs(A.Border), A.background];};


export  function areaAttsFromJs(A)  {sys.$params(arguments.length, 1);  return mkAreaAtts(lineFromJs(A[0]), A[1]);};


export  function mkAreaAttsExample()  {sys.$params(arguments.length, 0);  return mkAreaAtts(mkLineExample(), "#e9eaec");};





export  function mkArea(width, height, Atts)  {sys.$params(arguments.length, 3);  return {width:width, height:height, Atts:Atts};};


export  function areaCopy(A)  {sys.$params(arguments.length, 1);  return mkArea(A.width, A.height, areaAttsCopy(A.Atts));};


export  function areaToJs(A)  {sys.$params(arguments.length, 1);  return [A.width, A.height, areaAttsToJs(A.Atts)];};


export  function areaFromJs(A)  {sys.$params(arguments.length, 1);  return mkArea(A[0], A[1], areaAttsFromJs(A[2]));};


export  function mkAreaExample()  {sys.$params(arguments.length, 0);  return mkArea(400, 200, mkAreaAttsExample());};





export  function mkUnarySet(label, value, Line)  {sys.$params(arguments.length, 3);  return {label:label, value:value, Line:Line};};


export  function mkUnarySetExample()  {sys.$params(arguments.length, 0);  return mkUnarySet("Line", 0, mkLineExample());};

















































export  function mkData(Labels, SetValues, SetAtts)  {sys.$params(arguments.length, 3);
    if (sys.asBool(!sys.asBool(Labels))) throw new Error(("'Labels' does not have values"));
    if (sys.asBool(!sys.asBool(SetValues))) throw new Error(("'SetValuess' does not have values"));
    if (sys.asBool(!sys.asBool(SetValues[0]))) throw new Error(("'SetValuess[0]' does not have values"));
    if (sys.asBool(sys.$neq(arr.size(SetValues) , arr.size(SetAtts))))
      throw new Error( (
        "Number of sets (" + arr.size(SetValues) +
        ") does not match number of sets Attributes (" + arr.size(SetAtts) + ")"
      ));

    const UnarySets =sys.$checkNull( []);
    const round =sys.$checkNull( 2);
     function maxMinRound(mx, mn)  {sys.$params(arguments.length, 2);  return 0;};
     function drawGrid(l, i)  {sys.$params(arguments.length, 2);  return true;};
     function drawLabel(l, i)  {sys.$params(arguments.length, 2);  return true;};
     function mapLabel(l, i)  {sys.$params(arguments.length, 2);  return l;};

     return {
        Labels:Labels, SetValues:SetValues, SetAtts:SetAtts, UnarySets:UnarySets, round:round, maxMinRound:maxMinRound,
        drawGrid:drawGrid, drawLabel:drawLabel, mapLabel:mapLabel
      };
};


export  function mkDataExample()  {sys.$params(arguments.length, 0);
  const Labels =sys.$checkNull( ["Mon", "Tue", "Wen", "Thu", "Fri", "Sat", "Sun"]);
  const SetValues =sys.$checkNull( [
    arr.map([1, 2, 9.54, 10.2, 6.2,  -7, 7], function(e)  {sys.$params(arguments.length, 1);  return [e];}),
    arr.map([2,  -4,  -2.15,  -5.2, 7, 3, 4], function(e)  {sys.$params(arguments.length, 1);  return [e];})
  ]);
  const SetAtts =sys.$checkNull( [mkLineExample(),mkLineExample()]);
  SetAtts[0].color =sys.$checkExists(SetAtts[0].color,sys.$checkNull( "#000080"));
  SetAtts[1].color =sys.$checkExists(SetAtts[1].color,sys.$checkNull( "#800000"));

   return mkData(Labels, SetValues, SetAtts);
};







export  function mkLabels(show, onPopup)  {sys.$params(arguments.length, 2);  return {show:show, onPopup:onPopup};};


export  function mkLabelsExample()  {sys.$params(arguments.length, 0);  return mkLabels(true, false);};











export  function mkX(fontSize, isMonospace, isItalic, isBold, fontColor, Grid)  {sys.$params(arguments.length, 6);  return {
    fontSize:fontSize, isMonospace:isMonospace, isItalic:isItalic, isBold:isBold, fontColor:fontColor, Grid:Grid
  };};


export  function xCopy(X)  {sys.$params(arguments.length, 1);  return mkX(
    X.fontSize, X.isMonospace, X.isItalic, X.isBold, X.fontColor, X.Grid
  );};


export  function xToJs(X)  {sys.$params(arguments.length, 1);  return [
    X.fontSize, X.isMonospace, X.isItalic, X.isBold, X.fontColor, lineToJs(X.Grid)
  ];};


export  function xFromJs(A)  {sys.$params(arguments.length, 1);  return mkX(
    A[0], A[1], A[3], A[4], lineFromJs(A[5])
  );};


export  function mkXExample()  {sys.$params(arguments.length, 0);  return mkX(
    12, false, false, false, "#000000", mkLine(1, "#808080", true)
  );};












export  function mkY(fontSize, isMonospace, isItalic, isBold, fontColor, Grid, parts)  {sys.$params(arguments.length, 7);  return {
    fontSize:fontSize, isMonospace:isMonospace, isItalic:isItalic, isBold:isBold, fontColor:fontColor, Grid:Grid, parts:parts
  };};


export  function yCopy(Y)  {sys.$params(arguments.length, 1);  return mkY(
    Y.fontSize, Y.isMonospace, Y.isItalic, Y.isBold, Y.fontColor, Y.Grid, Y.parts
  );};


export  function yToJs(Y)  {sys.$params(arguments.length, 1);  return [
    Y.fontSize, Y.isMonospace, Y.isItalic, Y.isBold, Y.fontColor,
    lineToJs(Y.Grid), Y.parts
  ];};


export  function yFromJs(A)  {sys.$params(arguments.length, 1);  return mkY(
    A[0], A[1], A[3], A[4], lineFromJs(A[5]), A[6]
  );};


export  function mkYExample()  {sys.$params(arguments.length, 0);  return mkY(
    12, false, false, false, "#000000", mkLine(1, "#808080", true), 4
  );};













export  function mk(ExArea, InPadding, InAtts, ChartPadding, Labels, XAxis, YAxis, lang)  {sys.$params(arguments.length, 8);  return {
    ExArea:ExArea, InPadding:InPadding, InAtts:InAtts, ChartPadding:ChartPadding, Labels:Labels, XAxis:XAxis, YAxis:YAxis, lang:lang
  };};


export  function copy(lC)  {sys.$params(arguments.length, 1);  return mk(
    areaCopy(lC.ExArea), paddingCopy(lC.InPadding), areaAttsCopy(lC.InAtts),
    paddingCopy(lC.ChartPadding), xCopy(lC.XAxis), yCopy(lC.YAxis), lC.lang
  );};


export  function toJs(lC)  {sys.$params(arguments.length, 1);  return [
    areaToJs(lC.ExArea), paddingToJs(lC.InPadding), areaAttsToJs(lC.InAtts),
    paddingToJs(lC.ChartPadding), xToJs(lC.XAxis), yToJs(lC.YAxis), lC.lang
  ];};


export  function fromJs(A)  {sys.$params(arguments.length, 1);  return mk(
    areaFromJs(A[0]), paddingFromJs(A[1]), areaAttsFromJs(A[2]),
    paddingFromJs(A[3]), xFromJs(A[4]), yFromJs(A[5]), A[6]
  );};


export  function mkExample()  {sys.$params(arguments.length, 0);
  const InAtts =sys.$checkNull( mkAreaAttsExample());
  InAtts.background =sys.$checkExists(InAtts.background,sys.$checkNull( "#fbfdff"));
   return mk(
    mkAreaExample(),
    mkPaddingExample(),
    InAtts,
    mkPadding(2, 4, 2, 4),
    mkLabelsExample(),
    mkXExample(),
    mkYExample(),
    "es"
  );
};





export  function mkWg(lC, Data)  {sys.$params(arguments.length, 2);
  
  
   function corr(x)  {sys.$params(arguments.length, 1);  return Math.floor(x) + 0.5;};

  
   function decFmt(n)  {sys.$params(arguments.length, 1); return sys.asBool( sys.$eq(lC.lang , "es"))
      ? math.toIso(n, Data.round)
      : math.toEn(n, Data.round)
    ;};

  
  const HotLabels =sys.$checkNull( []);
  
  const HotUnarySets =sys.$checkNull( []);
  
  const HotSetValues =sys.$checkNull( []);

  
  const Max =sys.$checkNull( [[]]);
  const Min =sys.$checkNull( [0.0]);
  const Gap =sys.$checkNull( [0.0]);

  

  for (let S  of sys.$forObject( Data.SetValues)) for (let Val  of sys.$forObject( S)) {
    if (sys.asBool(Val)) {
      if (sys.asBool(Max[0])) {
        Max[0][0] =sys.$checkExists(Max[0][0],sys.$checkNull(sys.asBool( Val[0] > Max[0][0]) ? Val[0] : Max[0][0]));
        Min[0] =sys.$checkExists(Min[0],sys.$checkNull(sys.asBool( Val[0] < Min[0]) ? Val[0] : Min[0]));
      } else {
        arr.push(Max[0], Val[0]);
        Min[0] =sys.$checkExists(Min[0],sys.$checkNull( Val[0]));
      }
    }
  }
  for (let S  of sys.$forObject( Data.UnarySets)) {
    if (sys.asBool(Max[0])) {
      Max[0][0] =sys.$checkExists(Max[0][0],sys.$checkNull(sys.asBool( S.value > Max[0][0]) ? S.value : Max[0][0]));
      Min[0] =sys.$checkExists(Min[0],sys.$checkNull(sys.asBool( S.value < Min[0]) ? S.value : Min[0]));
    } else {
      arr.push(Max[0], S.value);
      Min[0] =sys.$checkExists(Min[0],sys.$checkNull( S.value));
    }
  }
  if (sys.asBool(Max[0])) {
    const round =sys.$checkNull( Math.pow(10, Data.maxMinRound(Max[0][0], Min[0])));
    Max[0][0] =sys.$checkExists(Max[0][0],sys.$checkNull( (Math.round(Max[0][0] / round) + 1) * round));
    Min[0] =sys.$checkExists(Min[0],sys.$checkNull( (Math.round(Min[0] / round) - 1) * round));
    Gap[0] =sys.$checkExists(Gap[0],sys.$checkNull( Max[0][0] - Min[0]));
  }

  

  const w =sys.$checkNull( lC.ExArea.width -
    lC.InPadding.left - lC.InPadding.right -
    lC.ChartPadding.left - lC.ChartPadding.right)
  ;
  const h =sys.$checkNull( lC.ExArea.height -
    lC.InPadding.top - lC.InPadding.bottom -
    lC.ChartPadding.top - lC.ChartPadding.bottom)
  ;
  const x0 =sys.$checkNull( lC.InPadding.left + lC.ChartPadding.left);
  const xEnd =sys.$checkNull( x0 + w);
  const y0 =sys.$checkNull( lC.ExArea.height - lC.InPadding.bottom - lC.ChartPadding.bottom);
  const yEnd =sys.$checkNull( y0 - h);

  

  const wg =sys.$checkNull( Q("div"));

  const cv =sys.$checkNull( Q("canvas")
    .att("width", lC.ExArea.width)
    .att("height", lC.ExArea.height)
    .style("background:" + lC.ExArea.Atts.background))
  ;
  const cv2 =sys.$checkNull( Q("canvas")
    .att("width", 0)
    .att("height", 0)
    .style(
        "border: 1px solid black;" +
        "background:" + lC.InAtts.background + ";" +
        "position: absolute;" +
        "visibility: hidden;"
      ))
  ;
  cv2.on("mousemove", function(ev)  {sys.$params(arguments.length, 1);
    if (sys.asBool(
      sys.asBool(ev.offsetX < cv2.getAtt("width") - 6) ||
      sys.asBool(ev.offsetY < cv2.getAtt("height") - 6))
    ) cv2.setStyle("visibility", "hidden");}
  );
  const ctx =sys.$checkNull( cv.e.getContext("2d"));

  

  if (sys.asBool(lC.ExArea.Atts.Border.width > 0)) {
    ctx.setLineDash(sys.asBool(lC.ExArea.Atts.Border.dotted) ? [4, 2] : []);
    ctx.lineWidth =sys.$checkExists(ctx.lineWidth,sys.$checkNull( lC.ExArea.Atts.Border.width));
    ctx.strokeStyle =sys.$checkExists(ctx.strokeStyle,sys.$checkNull( lC.ExArea.Atts.Border.color));
    ctx.beginPath();
    ctx.rect(
      corr(0),
      corr(0),
      Math.round(lC.ExArea.width - 1),
      Math.round(lC.ExArea.height - 1)
    );
    ctx.stroke();
  }

  

  const ilf =sys.$checkNull( lC.InPadding.left);
  const itop =sys.$checkNull( lC.InPadding.top);
  const iw =sys.$checkNull( lC.ExArea.width - lC.InPadding.left - lC.InPadding.right - 1);
  const ih =sys.$checkNull( lC.ExArea.height - lC.InPadding.top - lC.InPadding.bottom - 1);

  ctx.fillStyle =sys.$checkExists(ctx.fillStyle,sys.$checkNull( lC.InAtts.background));
  ctx.beginPath();
  ctx.rect(ilf, itop, iw, ih);
  ctx.fill();

  

  ctx.fillStyle =sys.$checkExists(ctx.fillStyle,sys.$checkNull( lC.XAxis.fontColor));
  ctx.font =sys.$checkExists(ctx.font,sys.$checkNull( "" + lC.XAxis.fontSize + "px " +
    (sys.asBool(lC.XAxis.isMonospace) ? "monospace" : "sans") +
    (sys.asBool(lC.XAxis.isItalic) ? " italic" : "") +
    (sys.asBool(lC.XAxis.isBold) ? " bold" : "")))
  ;
  for (let i  of sys.$forObject( iter.$range(0,arr.size(Data.Labels)))) {
    const l0 =sys.$checkNull( Data.Labels[i]);

    if (sys.asBool(!sys.asBool(Data.drawLabel(l0, i)))) continue;
    const l =sys.$checkNull( Data.mapLabel(l0, i));

    const lw =sys.$checkNull( ctx.measureText(l).width);
    ctx.fillText(l,
      x0  + lC.ChartPadding.left + i * w / (Data.Labels.length - 1) - lw / 2,
      y0 + lC.ChartPadding.bottom + lC.XAxis.fontSize
    );
  }

  for (let i  of sys.$forObject( iter.$range(0,arr.size(Data.Labels)))) {
    const l =sys.$checkNull( Data.Labels[i]);
    const cx =sys.$checkNull( corr(x0 + lC.ChartPadding.left + i * w / (arr.size(Data.Labels) - 1)));

    arr.push(HotLabels, cx);

    if (sys.asBool(sys.asBool(sys.asBool(sys.$eq(i , 0)) || sys.asBool(i >= arr.size(Data.Labels))) || sys.asBool(!sys.asBool(Data.drawGrid(l, i))))) continue;

    ctx.setLineDash(sys.asBool(lC.XAxis.Grid.dotted) ? [4, 2] : []);
    ctx.lineWidth =sys.$checkExists(ctx.lineWidth,sys.$checkNull( lC.XAxis.Grid.width));
    ctx.strokeStyle =sys.$checkExists(ctx.strokeStyle,sys.$checkNull( lC.XAxis.Grid.color));
    ctx.beginPath();
    ctx.moveTo(cx, corr(y0 + lC.ChartPadding.bottom));
    ctx.lineTo(cx, corr(yEnd - lC.ChartPadding.top));
    ctx.stroke();
  }

  

  ctx.fillStyle =sys.$checkExists(ctx.fillStyle,sys.$checkNull( lC.YAxis.fontColor));
  ctx.font =sys.$checkExists(ctx.font,sys.$checkNull( "" + lC.YAxis.fontSize + "px " +
    (sys.asBool(lC.YAxis.isMonospace) ? "monospace" : "sans") +
    (sys.asBool(lC.YAxis.isItalic) ? " italic" : "") +
    (sys.asBool(lC.YAxis.isBold) ? " bold" : "")))
  ;

  const parts =sys.$checkNull(sys.asBool( lC.YAxis.parts < 1) ? 1 : lC.YAxis.parts);
  for (let i  of sys.$forObject( iter.$range(0,parts+1))) {
    const yVal =sys.$checkNull( Min[0] + i * Gap[0] / parts);
    const y =sys.$checkNull( y0 - (yVal - Min[0]) * h / Gap[0]);

    const n =sys.$checkNull( decFmt(yVal));
    const ms =sys.$checkNull( ctx.measureText(n).width);
    ctx.fillText(
      n,
      lC.InPadding.left - 4 - ms,
      y + lC.YAxis.fontSize / 2.5
    );

    if (sys.asBool(sys.asBool(sys.$eq(i , 0)) || sys.asBool(sys.$eq(i , parts)))) continue;

    ctx.setLineDash(sys.asBool(lC.YAxis.Grid.dotted) ? [4, 2] : []);
    ctx.lineWidth =sys.$checkExists(ctx.lineWidth,sys.$checkNull( lC.YAxis.Grid.width));
    ctx.strokeStyle =sys.$checkExists(ctx.strokeStyle,sys.$checkNull( lC.YAxis.Grid.color));
    ctx.beginPath();
    ctx.moveTo(x0 - lC.ChartPadding.left, corr(y));
    ctx.lineTo(xEnd + lC.ChartPadding.right, corr(y));
    ctx.stroke();
  }

  
  for (let Ul  of sys.$forObject( Data.UnarySets)) {
    const cy =sys.$checkNull( y0 - (Ul.value - Min[0]) * h / Gap[0]);
    arr.push(HotUnarySets, corr(cy));

    ctx.setLineDash(sys.asBool(Ul.Line.dotted) ? [4, 2] : []);
    ctx.lineWidth =sys.$checkExists(ctx.lineWidth,sys.$checkNull( Ul.Line.width));
    ctx.strokeStyle =sys.$checkExists(ctx.strokeStyle,sys.$checkNull( Ul.Line.color));
    ctx.beginPath();
    ctx.moveTo(corr(x0), corr(cy));
    ctx.lineTo(corr(xEnd), corr(cy));
    ctx.stroke();
  }

  

  if (sys.asBool(Max[0])) {
    for (let i  of sys.$forObject( iter.$range(0,arr.size(Data.SetValues)))) {
      const S =sys.$checkNull( Data.SetValues[i]);
      const sSize =sys.$checkNull( arr.size(S));
      
      const HotSetRow =sys.$checkNull( []);

      const Cy0 =sys.$checkNull( [0]);
      const IxStart =sys.$checkNull( [0]);
      for (let j  of sys.$forObject( iter.$range(0,sSize))) {
        const Sval =sys.$checkNull( S[j]);
        if (sys.asBool(!sys.asBool(Sval))) {
          arr.push(HotSetRow, []);
          continue;
        }
        IxStart[0] =sys.$checkExists(IxStart[0],sys.$checkNull( j + 1));
        Cy0[0] =sys.$checkExists(Cy0[0],sys.$checkNull( corr(y0 - (Sval[0] - Min[0]) * h / Gap[0])));
        arr.push(HotSetRow, [Cy0[0]]);
        break;
      }

      ctx.setLineDash(sys.asBool(Data.SetAtts[i].dotted) ? [4, 2] : []);
      ctx.lineWidth =sys.$checkExists(ctx.lineWidth,sys.$checkNull( Data.SetAtts[i].width));
      ctx.strokeStyle =sys.$checkExists(ctx.strokeStyle,sys.$checkNull( Data.SetAtts[i].color));
      ctx.beginPath();
      ctx.moveTo(corr(x0 + (IxStart[0] - 1) * w / (sSize - 1)), Cy0[0]);
      if (sys.asBool(sys.asBool(IxStart[0] >= sSize) || sys.asBool(!sys.asBool(S[IxStart[0]])))) {
        ctx.arc(
          corr(x0 + (IxStart[0] - 1) * w / (sSize - 1)), Cy0[0],
          ctx.lineWidth / 2, 0, 2 * Math.PI
        );
      }
      const J =sys.$checkNull( [IxStart[0]]);
      while (sys.asBool(J[0] < sSize)) {
        if (sys.asBool(S[J[0]])) {
          const v =sys.$checkNull( S[J[0]][0]);
          const cy =sys.$checkNull( corr(y0 - (v - Min[0]) * h / Gap[0]));
          arr.push(HotSetRow, [cy]);
          ctx.lineTo(corr(x0 + J[0] * w / (sSize - 1)), cy);
          J[0] +=sys.$checkExists(J[0],sys.$checkNull( 1));
        } else {
          arr.push(HotSetRow, []);
          J[0] +=sys.$checkExists(J[0],sys.$checkNull( 1));
          while (sys.asBool(J[0] < sSize)) {
            if (sys.asBool(S[J[0]])) {
              const v =sys.$checkNull( S[J[0]][0]);
              const cy =sys.$checkNull( corr(y0 - (v - Min[0]) * h / Gap[0]));
              arr.push(HotSetRow, [cy]);
              ctx.moveTo(corr(x0 + J[0] * w / (sSize - 1)), cy);
              J[0] +=sys.$checkExists(J[0],sys.$checkNull( 1));
              if (sys.asBool(sys.asBool(J[0] >= sSize) || sys.asBool(!sys.asBool(S[J[0]])))) {
                ctx.arc(
                  corr(x0 + (J[0] - 1) * w / (sSize - 1)), cy,
                  ctx.lineWidth / 2, 0, 2 * Math.PI
                );
              }
              break;
            } else {
              arr.push(HotSetRow, []);
              J[0] +=sys.$checkExists(J[0],sys.$checkNull( 1));
            }
          }
        }
      }
      ctx.stroke();

      arr.push(HotSetValues, HotSetRow);
    }
  }

  

  if (sys.asBool(lC.InAtts.Border.width > 0)) {
    ctx.setLineDash(sys.asBool(lC.InAtts.Border.dotted) ? [4, 2] : []);
    ctx.lineWidth =sys.$checkExists(ctx.lineWidth,sys.$checkNull( lC.InAtts.Border.width));
    ctx.strokeStyle =sys.$checkExists(ctx.strokeStyle,sys.$checkNull( lC.InAtts.Border.color));
    ctx.beginPath();
    ctx.rect(corr(ilf), corr(itop), Math.round(iw), Math.round(ih));
    ctx.stroke();
  }

  

  cv.on("mousemove", function(ev)  {sys.$params(arguments.length, 1);
    const cx =sys.$checkNull( ev.offsetX);
    const cy =sys.$checkNull( ev.offsetY);

    
    

    const UnarySetIx =sys.$checkNull( [ -1]);
    const UnarySetDif =sys.$checkNull( [0]);
    for (let i  of sys.$forObject( iter.$range(0,arr.size(HotUnarySets)))) {
      const v =sys.$checkNull( HotUnarySets[i]);
      const dif =sys.$checkNull( Math.abs(v - cy));
      if (sys.asBool(sys.asBool(dif < 4) && sys.asBool((sys.asBool(sys.$eq(UnarySetIx[0] ,  -1)) || sys.asBool(dif < UnarySetDif[0]))))) {
        UnarySetIx[0] =sys.$checkExists(UnarySetIx[0],sys.$checkNull( i));
        UnarySetDif[0] =sys.$checkExists(UnarySetDif[0],sys.$checkNull( dif));
      }
    }

    const SetIx =sys.$checkNull( [ -1]);
    const SetValIx =sys.$checkNull( [ -1]);
    const SetDif =sys.$checkNull( [0]);
    for (let i  of sys.$forObject( iter.$range(0,arr.size(Data.SetValues)))) {
      if (sys.asBool(sys.asBool(i < 0) || sys.asBool(i > arr.size(HotSetValues)))) continue;
      const HotSetValuesRow =sys.$checkNull( HotSetValues[i]);
      const Vs =sys.$checkNull( Data.SetValues[i]);
      for (let j  of sys.$forObject( iter.$range(0,arr.size(Vs)))) {
        if (sys.asBool(sys.asBool(j < 0) || sys.asBool(j > arr.size(HotSetValuesRow)))) continue;
        const HotSet =sys.$checkNull( HotSetValuesRow[j]);
        if (sys.asBool(!sys.asBool(HotSet))) continue;

        const xdif =sys.$checkNull( HotLabels[j] - cx);
        const ydif =sys.$checkNull( HotSet[0] - cy);
        const dif =sys.$checkNull( Math.sqrt(xdif * xdif + ydif * ydif));

        if (sys.asBool(sys.asBool(dif < 4) && sys.asBool((sys.asBool(sys.$eq(SetIx[0] ,  -1)) || sys.asBool(dif <= SetDif[0]))))) {
          SetIx[0] =sys.$checkExists(SetIx[0],sys.$checkNull( i));
          SetValIx[0] =sys.$checkExists(SetValIx[0],sys.$checkNull( j));
          SetDif[0] =sys.$checkExists(SetDif[0],sys.$checkNull( dif));
        }
      }
    }

    if (sys.asBool(sys.asBool(sys.$neq(UnarySetIx[0] ,  -1)) || sys.asBool(sys.$neq(SetIx[0] ,  -1)))) {
      const yfirst =sys.$checkNull( lC.YAxis.fontSize);
      const ysecond =sys.$checkNull( yfirst * 2 + yfirst / 4);
      const ysize =sys.$checkNull( yfirst * 2.75);

      const Tx1 =sys.$checkNull( [""]);
      const Tx2 =sys.$checkNull( [""]);
      const Color =sys.$checkNull( [""]);

      if (sys.asBool(sys.$neq(SetIx[0] ,  -1))) {
        Tx1[0] =sys.$checkExists(Tx1[0],sys.$checkNull( Data.Labels[SetValIx[0]]));
        Tx2[0] =sys.$checkExists(Tx2[0],sys.$checkNull( decFmt(Data.SetValues[SetIx[0]][SetValIx[0]][0])));
        Color[0] =sys.$checkExists(Color[0],sys.$checkNull( Data.SetAtts[SetIx[0]].color));
      } else {
        Tx1[0] =sys.$checkExists(Tx1[0],sys.$checkNull( Data.UnarySets[UnarySetIx[0]].label));
        Tx2[0] =sys.$checkExists(Tx2[0],sys.$checkNull( decFmt(Data.UnarySets[UnarySetIx[0]].value)));
        Color[0] =sys.$checkExists(Color[0],sys.$checkNull( Data.UnarySets[UnarySetIx[0]].Line.color));
      }

      const ctx2 =sys.$checkNull( cv2.e.getContext("2d"));
      ctx2.font =sys.$checkExists(ctx2.font,sys.$checkNull( "" + lC.YAxis.fontSize + "px " +
        (sys.asBool(lC.YAxis.isMonospace) ? "monospace" : "sans") +
        (sys.asBool(lC.YAxis.isItalic) ? " italic" : "") +
        (sys.asBool(lC.YAxis.isBold) ? " bold" : "")))
      ;
      const ms1 =sys.$checkNull( ctx2.measureText(Tx1[0]).width);
      const ms2 =sys.$checkNull( ctx2.measureText(Tx2[0]).width);

      const Margin1 =sys.$checkNull( [4.0]);
      const Margin2 =sys.$checkNull( [Math.abs(ms1 - ms2) / 2 + Margin1[0]]);
      const Ms =sys.$checkNull( [ms1 + Margin1[0] * 2]);
      if (sys.asBool(ms2 > ms1)) {
        Margin1[0] =sys.$checkExists(Margin1[0],sys.$checkNull( Margin2[0]));
        Margin2[0] =sys.$checkExists(Margin2[0],sys.$checkNull( 4));
        Ms[0] =sys.$checkExists(Ms[0],sys.$checkNull( ms2 + Margin2[0] * 2));
      }

      if (sys.asBool(lC.Labels.show)) {
        const posY =sys.$checkNull(sys.asBool( lC.Labels.onPopup) ? ev.clientY : ui.mouseY(ev));
        const posX =sys.$checkNull(sys.asBool( lC.Labels.onPopup) ? ev.clientX : ui.mouseX(ev));
        cv2
          .att("height", ysize)
          .att("width", Ms[0])
          .setStyle("top", "" + (posY - ysize) + "px")
          .setStyle("left", "" + (posX - Ms[0]) + "px")
          .setStyle("visibility", "visible")
        ;

        const ctx3 =sys.$checkNull( cv2.e.getContext("2d"));
        ctx3.fillStyle =sys.$checkExists(ctx3.fillStyle,sys.$checkNull( Color[0]));
        ctx3.font =sys.$checkExists(ctx3.font,sys.$checkNull( "" + lC.YAxis.fontSize + "px " +
          (sys.asBool(lC.YAxis.isMonospace) ? "monospace" : "sans") +
          (sys.asBool(lC.YAxis.isItalic) ? " italic" : "") +
          (sys.asBool(lC.YAxis.isBold) ? " bold" : "")))
        ;
        ctx3.fillText(Tx1[0], Margin1[0], yfirst);
        ctx3.fillText(Tx2[0], Margin2[0], ysecond);
      }
    } else {
      cv2.setStyle("visibility", "hidden");
    }
  });

   return wg.add(cv).add(cv2);
};


export  function mkWgExample() {sys.$params(arguments.length, 0);  return mkWg(mkExample(), mkDataExample());};
