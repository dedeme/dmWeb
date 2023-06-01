import * as iter from '../_js/iter.js';import * as str from '../_js/str.js';import * as bytes from '../_js/bytes.js';import * as cryp from '../_js/cryp.js';import * as dic from '../_js/dic.js';import * as timer from '../_js/timer.js';import * as js from '../_js/js.js';import * as storage from '../_js/storage.js';import * as sys from '../_js/sys.js';import * as math from '../_js/math.js';import * as domo from '../_js/domo.js';import * as ui from '../_js/ui.js';import * as arr from '../_js/arr.js';import * as time from '../_js/time.js';import * as client from '../_js/client.js';import * as b64 from '../_js/b64.js';




import * as modalBox from  "../libdm/modalBox.js";
import * as cts from  "../data/cts.js";
import * as image from  "../data/image.js";
import * as imageViewer from  "../wgs/imageViewer.js";
import * as cutEditor from  "../wgs/cutEditor.js";
import * as adjustmentEditor from  "../wgs/adjustmentEditor.js";
import * as blurEditor from  "../wgs/blurEditor.js";
import * as i18n from  "../i18n.js";

const Q =sys.$checkNull( ui.q);
const II =sys.$checkNull( i18n.tlt);


export  function mk(Images, ix)  {sys.$params(arguments.length, 2);
  const Image =sys.$checkNull( [Images[ix]]);

  const OptionsDiv =sys.$checkNull( Q("div"));
  const SourceDiv =sys.$checkNull( Q("div"));
  const TargetDiv =sys.$checkNull( Q("div"));
  const ImgEditorDiv =sys.$checkNull( Q("div"));

  const UpdateImage =sys.$checkNull( [[]]);

  

  
   function editCut(ev)  {sys.$params(arguments.length, 1);
    cutEditor.mk(ImgEditorDiv, Image[0], UpdateImage[0]);
    modalBox.show(cts.Box, true);
  };

  
   function editAdjustment(ev)  {sys.$params(arguments.length, 1);
    adjustmentEditor.mk(ImgEditorDiv, Image[0], UpdateImage[0]);
    modalBox.show(cts.Box, true);
  };

  
   function editBlur(ev)  {sys.$params(arguments.length, 1);
    blurEditor.mk(ImgEditorDiv, Image[0], UpdateImage[0]);
    modalBox.show(cts.Box, true);
  };

  

  
   function showSourceImg()  {sys.$params(arguments.length, 0);
     function action()  {sys.$params(arguments.length, 0);
      imageViewer.mk(
        cts.BoxContent, "source/" + Image[0].id, 800,
        function()  {sys.$params(arguments.length, 0); modalBox.show(cts.Box, false);}
      );
      modalBox.show(cts.Box, true);
    };

    imageViewer.mk(SourceDiv, "source/" + Image[0].id, 240, action);
  };

  
   function showOptions()  {sys.$params(arguments.length, 0);
    OptionsDiv
      .removeAll()
      .add(Q("table")
        .add(Q("tr")
          .add(Q("td")
            .add(Q("input")
              .att("type", "checkbox")
              .disabled(true)
              .checked(sys.asBool(true) && sys.asBool(Image[0].Cut))))
          .add(Q("td")
            .add(ui.link(editCut)
              .klass("link")
              .text(II("Cut")))))
        .add(Q("tr")
          .add(Q("td")
            .add(Q("input")
              .att("type", "checkbox")
              .disabled(true)
              .checked(sys.asBool(true) && sys.asBool(Image[0].Adjustment))))
          .add(Q("td")
            .add(ui.link(editAdjustment)
              .klass("link")
              .text(II("Adjustment")))))
        .add(Q("tr")
          .add(Q("td")
            .add(Q("input")
              .att("type", "checkbox")
              .disabled(true)
              .checked(sys.asBool(true) && sys.asBool(Image[0].Blur))))
          .add(Q("td")
            .add(ui.link(editBlur)
              .klass("link")
              .text(II("Blur")))))
      )
    ;
  };

  
   function showTargetImg()  {sys.$params(arguments.length, 0);
    const ix =sys.$checkNull( str.lastIndex(Image[0].id, "."));
    const newId =sys.$checkNull(sys.asBool( sys.$eq(ix ,  -1))
      ? Image[0].id + ".jpg"
      : sys.$slice(Image[0].id,null,ix) + ".jpg")
    ;

     function action()  {sys.$params(arguments.length, 0);
      imageViewer.mk(
        cts.BoxContent, "target/" + newId, 800,
        function()  {sys.$params(arguments.length, 0); modalBox.show(cts.Box, false);}
      );
      modalBox.show(cts.Box, true);
    };

    imageViewer.mk(TargetDiv, "target/" + newId, 240, action);
    imageViewer.mk(ImgEditorDiv, "target/" + newId, 800, function()  {sys.$params(arguments.length, 0);});
  };

  
   async  function showTarget()  {sys.$params(arguments.length, 0);
    OptionsDiv.removeAll().add(ui.img("wait.gif"));
    TargetDiv.removeAll().add(ui.img("wait.gif"));
    ImgEditorDiv.removeAll().add(ui.img("wait.gif"));
    const Rp =sys.$checkNull( await  client.send({
      prg: cts.appName,
      source: "Tr",
      rq: "idata",
      image: image.toJs(Image[0])
    }));
    if (sys.asBool(!sys.asBool(Rp.ok))) {
      ui.alert(i18n.fmt(II("Image '%0' can not be processed"), [Image[0].id]));
      window.location.reload(true);
    } else {
      showOptions();
      showTargetImg();
    }
  };

  
  UpdateImage[0] =sys.$checkExists(UpdateImage[0], async  function(Img)  {sys.$params(arguments.length, 1);
    Image[0] =sys.$checkExists(Image[0],sys.$checkNull( Img));
    Images[ix] =sys.$checkExists(Images[ix],sys.$checkNull( Img));
    await client.send({
      prg: cts.appName,
      source: "Tr",
      rq: "update",
      images: arr.map(Images, image.toJs)
    });
    showTarget();
  });

  
   function update()  {sys.$params(arguments.length, 0);
    showSourceImg();
    showTarget();
  };

  update();
   return Q("tr")
    .add(Q("td")
      .style("text-align:left;vertical-align:middle")
      .add(OptionsDiv))
    .add(Q("td").klass("separator"))
    .add(Q("td")
      .style("text-align:center;vertical-align:middle")
      .add(SourceDiv))
    .add(Q("td").klass("separator"))
    .add(Q("td")
      .style("text-align:center;vertical-align:middle")
      .add(TargetDiv))
  ;

};
