import * as iter from '../_js/iter.js';import * as str from '../_js/str.js';import * as bytes from '../_js/bytes.js';import * as cryp from '../_js/cryp.js';import * as dic from '../_js/dic.js';import * as timer from '../_js/timer.js';import * as js from '../_js/js.js';import * as storage from '../_js/storage.js';import * as sys from '../_js/sys.js';import * as math from '../_js/math.js';import * as domo from '../_js/domo.js';import * as ui from '../_js/ui.js';import * as arr from '../_js/arr.js';import * as time from '../_js/time.js';import * as client from '../_js/client.js';import * as b64 from '../_js/b64.js';




import * as modalBox from  "../libdm/modalBox.js";
import * as cts from  "../data/cts.js";
import * as fns from  "../data/fns.js";
import * as image from  "../data/image.js";
import * as imgAdjustment from  "../data/imgAdjustment.js";
import * as imageViewer from  "../wgs/imageViewer.js";
import * as i18n from  "../i18n.js";

const Q =sys.$checkNull( ui.q);
const II =sys.$checkNull( i18n.tlt);


export  function mk(ImgDiv, Image, onChange)  {sys.$params(arguments.length, 3);
  const Adj =sys.$checkNull( Image.Adjustment);
  const State =sys.$checkNull( [sys.asBool(Adj) ? Adj[0].type : ""]);

  
   function mkIn(id, nextId, value)  {sys.$params(arguments.length, 3);  return ui.changePoint(ui.field(nextId)
      .att("id", id)
      .style("width:80px")
      .value(value)
    );};

  const StartCut =sys.$checkNull( mkIn(
    "startCut", "startCut",sys.asBool(
    sys.$eq(State[0] , "cut")) ? Adj[0].Params[0] : 0
  ));
  const RatioBlur =sys.$checkNull( mkIn(
    "ratioBlur", "ratioLight",sys.asBool(
    sys.$eq(State[0] , "background")) ? Adj[0].Params[1] : 50
  ));
  const RatioLight =sys.$checkNull( mkIn(
    "ratioLight", "ratioBlur",sys.asBool(
    sys.$eq(State[0] , "background")) ? Adj[0].Params[2] : 30
  ));
  const PixelsStretch =sys.$checkNull( mkIn(
    "pixelsStretch", "pixelsStretch",sys.asBool(
    sys.$eq(State[0] , "stretch")) ? Adj[0].Params[0] : 10
  ));

  const Color =sys.$checkNull( Q("input")
    .att("type", "color")
    .value(sys.asBool(sys.$eq(State[0] , "background"))
        ? "#" + fns.intToColor(Adj[0].Params[0])
        : "#ffffff"
      ))
  ;

  const EditorDiv =sys.$checkNull( Q("div"));

  const MkEditor =sys.$checkNull( [[]]);

  

  
   function setState(v)  {sys.$params(arguments.length, 1);
    State[0] =sys.$checkExists(State[0],sys.$checkNull( v));
    MkEditor[0](EditorDiv);
  };

  
   function restore(ev)  {sys.$params(arguments.length, 1);
    const Adj =sys.$checkNull( Image.Adjustment);
    State[0] =sys.$checkExists(State[0],sys.$checkNull(sys.asBool( Adj) ? Adj[0].type : ""));

    StartCut.value(sys.asBool(sys.$eq(State[0] , "cut")) ?Adj[0].Params[0] : 0);
    RatioBlur.value(sys.asBool(sys.$eq(State[0] , "background")) ?Adj[0].Params[1] : 50);
    RatioLight.value(sys.asBool(sys.$eq(State[0] , "background")) ?Adj[0].Params[2] : 30);
    PixelsStretch.value(sys.asBool(sys.$eq(State[0] , "stretch")) ?Adj[0].Params[0] : 10);
    Color.value(sys.asBool(
      sys.$eq(State[0] , "background")) ? fns.intToColor(Adj[0].Params[0]) : "#ffffff"
    );

    onChange(Image);
    MkEditor[0](EditorDiv);
  };

  
   function update(ev)  {sys.$params(arguments.length, 1);
    
     function value(I, min, max, def)  {sys.$params(arguments.length, 4);
      const R =sys.$checkNull( math.fromStr(I.getValue()));
      if (sys.asBool(!sys.asBool(R))) {
        arr.push(R, def);
        I.value("" + R[0]);
      } else if (sys.asBool(R[0] > max)) {
        R[0] =sys.$checkExists(R[0],sys.$checkNull( max));
        I.value("" + R[0]);
      } else if (sys.asBool(R[0] < min)) {
        R[0] =sys.$checkExists(R[0],sys.$checkNull( min));
        I.value("" + R[0]);
      }
       return R[0];
    };
    const Adj =sys.$checkNull(   
      sys.$eq(State[0],"cut")? [imgAdjustment.mk(State[0], [value(StartCut, 0, 10000, 0)])]:
      sys.$eq(State[0],"background")? [
          imgAdjustment.mk(
            State[0],
            [ fns.colorToInt(sys.$slice(Color.getValue(),1,null)),
              value(RatioBlur, 0, 100, 50),
              value(RatioLight, 0, 100, 30)
            ]
          )
        ]:
      sys.$eq(State[0],"stretch")? [imgAdjustment.mk(State[0], [value(PixelsStretch, 1, 100, 10)])]:
       []
    );

    onChange(image.setAdjustment(Image, Adj));
    MkEditor[0](EditorDiv);
  };

  
   function close(ev)  {sys.$params(arguments.length, 1); modalBox.show(cts.Box, false);};

  

  
  MkEditor[0] =sys.$checkExists(MkEditor[0], function(Div)  {sys.$params(arguments.length, 1);
    const DeactivateBt =sys.$checkNull( Q("input")
      .att("type", "radio")
      .att("name", "type")
      .checked(sys.$eq(State[0] , ""))
      .on("click", function(ev)  {sys.$params(arguments.length, 1); setState("");}))
    ;
    const CutBt =sys.$checkNull( Q("input")
      .att("type", "radio")
      .att("name", "type")
      .checked(sys.$eq(State[0] , "cut"))
      .on("click", function(ev)  {sys.$params(arguments.length, 1); setState("cut");}))
    ;
    const BackgroundBt =sys.$checkNull( Q("input")
      .att("type", "radio")
      .att("name", "type")
      .checked(sys.$eq(State[0] , "background"))
      .on("click", function(ev)  {sys.$params(arguments.length, 1); setState("background");}))
    ;
    const StretchBt =sys.$checkNull( Q("input")
      .att("type", "radio")
      .att("name", "type")
      .checked(sys.$eq(State[0] , "stretch"))
      .on("click", function(ev)  {sys.$params(arguments.length, 1); setState("stretch");}))
    ;
    const Left =sys.$checkNull( Q("div")
      .add(Q("table")
        .add(Q("tr")
          .add(Q("td")
            .add(DeactivateBt))
          .add(Q("td")
            .style("text-align:left")
            .text(II("Deactivate"))))
        .add(Q("tr")
          .add(Q("td")
            .add(CutBt))
          .add(Q("td")
            .style("text-align:left")
            .text(II("Cut"))))
        .add(Q("tr")
          .add(Q("td")
            .add(BackgroundBt))
          .add(Q("td")
            .style("text-align:left")
            .text(II("Background"))))
        .add(Q("tr")
          .add(Q("td")
            .add(StretchBt))
          .add(Q("td")
            .style("text-align:left")
            .text(II("Stretch"))))
      ))
    ;
    const Right =sys.$checkNull( Q("div"));
    if (sys.asBool(sys.$eq(State[0] , "cut"))) {
      Right
        .removeAll()
        .add(Q("table")
          .att("align", "center")
          .add(Q("tr")
            .add(Q("td")
              .text(II("Pixels from top / left"))))
          .add(Q("tr")
            .add(StartCut)))
      ;
    } else if (sys.asBool(sys.$eq(State[0] , "background"))) {
      Right
        .removeAll()
        .add(Q("table")
          .att("align", "center")
          .add(Q("tr")
            .add(Q("td")
              .text(II("Color")))
            .add(Q("td")
              .text(II("Blur (0 - 100)")))
            .add(Q("td")
              .text(II("Light (0 - 100)"))))
          .add(Q("tr")
            .add(Q("td")
              .add(Color))
            .add(Q("td")
              .add(RatioBlur))
            .add(Q("td")
              .add(RatioLight))))
      ;
    } else if (sys.asBool(sys.$eq(State[0] , "stretch"))) {
      Right
        .removeAll()
        .add(Q("table")
          .att("align", "center")
          .add(Q("tr")
            .add(Q("td")
              .text(II("Pixels to sample"))))
          .add(Q("tr")
            .add(PixelsStretch)))
      ;
    } else {
      Right.removeAll();
    }

    Div
      .removeAll()
      .add(Q("table")
        .klass("main")
        .add(Q("tr")
          .add(Q("td")
            .style("width:5px;text-align:left")
            .add(Left))
          .add(Q("td")
            .add(Right))))
    ;
  });


  MkEditor[0](EditorDiv);

  cts.BoxContent
    .removeAll()
    .add(Q("table")
      .klass("main")
      .add(Q("tr")
        .add(Q("td"))
        .add(Q("td")
          .style("text-align:right")
          .add(ui.link(close)
            .klass("link")
            .text(II("Close")))))
      .add(Q("tr")
        .add(Q("td")
          .style("text-align:left")
          .add(ui.link(update)
            .klass("link")
            .text(II("Update")))
          .add(Q("span")
            .html("&nbsp;&nbsp;"))
          .add(ui.link(restore)
            .klass("link")
            .text(II("Restore"))))
        .add(Q("td"))))
    .add(Q("hr"))
    .add(EditorDiv)
    .add(Q("hr"))
    .add(ImgDiv)
  ;
};
