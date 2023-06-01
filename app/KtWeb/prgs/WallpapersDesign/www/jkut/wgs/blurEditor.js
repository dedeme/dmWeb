import * as iter from '../_js/iter.js';import * as str from '../_js/str.js';import * as bytes from '../_js/bytes.js';import * as cryp from '../_js/cryp.js';import * as dic from '../_js/dic.js';import * as timer from '../_js/timer.js';import * as js from '../_js/js.js';import * as storage from '../_js/storage.js';import * as sys from '../_js/sys.js';import * as math from '../_js/math.js';import * as domo from '../_js/domo.js';import * as ui from '../_js/ui.js';import * as arr from '../_js/arr.js';import * as time from '../_js/time.js';import * as client from '../_js/client.js';import * as b64 from '../_js/b64.js';




import * as modalBox from  "../libdm/modalBox.js";
import * as cts from  "../data/cts.js";
import * as image from  "../data/image.js";
import * as imgBlur from  "../data/imgBlur.js";
import * as i18n from  "../i18n.js";

const Q =sys.$checkNull( ui.q);
const II =sys.$checkNull( i18n.tlt);


export  function mk(ImgDiv, Image, onChange)  {sys.$params(arguments.length, 3);
  const Blur =sys.$checkNull( Image.Blur);
  const IsActivated =sys.$checkNull( [sys.asBool(Blur) ? true : false]);

  
   function mkIn(id, nextId, value)  {sys.$params(arguments.length, 3);  return ui.changePoint(ui.field(nextId)
      .att("id", id)
      .style("width:80px")
      .value(value)
    );};

  const Ratio =sys.$checkNull( mkIn("ratio", "ratio",sys.asBool( IsActivated[0]) ? Blur[0].ratio : cts.ratioBlur));

  const EditorDiv =sys.$checkNull( Q("div"));

  const MkEditor =sys.$checkNull( [[]]);

  

  
   function activate(value)  {sys.$params(arguments.length, 1);
    IsActivated[0] =sys.$checkExists(IsActivated[0],sys.$checkNull( value));
    MkEditor[0](EditorDiv);
  };

  
   function restore(ev)  {sys.$params(arguments.length, 1);
    const Blur =sys.$checkNull( Image.Blur);
    const IsActivated =sys.$checkNull( [sys.asBool(Blur) ? true : false]);

    Ratio.value(sys.asBool(IsActivated[0]) ? Blur.ratio : cts.ratioBlur);

    onChange(Image);
    MkEditor[0](EditorDiv);
  };

  
   function update(ev)  {sys.$params(arguments.length, 1);
    
     function value(I)  {sys.$params(arguments.length, 1);
      const R =sys.$checkNull( math.fromStr(I.getValue()));
      if (sys.asBool(!sys.asBool(R))) {
        arr.push(R, cts.ratioBlur);
        I.value("" + R[0]);
      }
       return R[0];
    };
    const Blur =sys.$checkNull(sys.asBool( IsActivated[0])
      ? [imgBlur.mk(value(Ratio))]
      : [])
    ;

    onChange(image.setBlur(Image, Blur));
    MkEditor[0](EditorDiv);
  };

  
   function close(ev)  {sys.$params(arguments.length, 1); modalBox.show(cts.Box, false);};

  

  
  MkEditor[0] =sys.$checkExists(MkEditor[0], function(Div)  {sys.$params(arguments.length, 1);
    Ratio.disabled(!sys.asBool(IsActivated[0]));
    Div
      .removeAll()
      .add(Q("table")
        .att("align", "center")
        .add(Q("tr")
          .add(Q("td")
            .att("colspan", 3)
            .add(sys.asBool(IsActivated[0])
                ? ui.link(function(ev)  {sys.$params(arguments.length, 1); activate(false);})
                    .klass("link")
                    .text(II("Deactivate"))
                : ui.link(function(ev)  {sys.$params(arguments.length, 1); activate(true);})
                    .klass("link")
                    .text(II("Activate"))
              )))
        .add(Q("tr")
          .add(Q("td")
            .style("text-align:center")
            .text(II("Percentage (0 - 100)"))))
        .add(Q("tr")
          .add(Q("td")
            .add(Ratio))))
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
