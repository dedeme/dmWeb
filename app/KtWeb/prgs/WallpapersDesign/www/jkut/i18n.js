import * as iter from './_js/iter.js';import * as str from './_js/str.js';import * as bytes from './_js/bytes.js';import * as cryp from './_js/cryp.js';import * as dic from './_js/dic.js';import * as timer from './_js/timer.js';import * as js from './_js/js.js';import * as storage from './_js/storage.js';import * as sys from './_js/sys.js';import * as math from './_js/math.js';import * as domo from './_js/domo.js';import * as ui from './_js/ui.js';import * as arr from './_js/arr.js';import * as time from './_js/time.js';import * as client from './_js/client.js';import * as b64 from './_js/b64.js';



const esDic =sys.$checkNull( {
  "Accept": "Aceptar",
  "Activate": "Activar",
  "Adjustment": "Ajuste",
  "Background": "Fondo",
  "Blur": "Desenfoque",
  "Blur (0 - 100)": "Desenfoque (0 - 100)",
  "Bottom": "Abajo",
  "Cancel": "Cancelar",
  "Click %0 to continue.": "Click %0 para continuar.",
  "Close": "Cerrar",
  "Color": "Color",
  "Cut": "Recorte",
  "Deactivate": "Desactivar",
  "Image '%0' can not be processed": "La imagen '%0' no puede ser procesada",
  "KtWeb session is closed.\nAuthenticating from KtWeb:Main.": "La sesión de KtWeb ha sido cerrada.\nHay que autenticarse en KtWeb:Main.",
  "Left": "Izquierda",
  "Light (0 - 100)": "Luz (0 - 100)",
  "Percentage (0 - 100)": "Porcentaje (0 - 100)",
  "Pixels from top / left": "Pixles desde arriba / izquierda",
  "Pixels to sample": "Pixels para muestra",
  "Restore": "Restaurar",
  "Right": "Derecha",
  "Save in Group %0": "Guadar en el grupo %0",
  "Save pictures in group %0?": "¿Guardar las imágenes en el grupo %0?",
  "Session is expired.": "Las sesión ha expirado.",
  "Stretch": "Estirar",
  "There are duplicated pictures": "Hay imágenes repetidas",
  "There are no pictures to adjust": "No hay imágenes para ajustar",
  "Top": "Arriba",
  "Update": "Actualizar",
  "here": "aquí"
});

export  function es() {sys.$params(arguments.length, 0); Lang[0] =sys.$checkExists(Lang[0],sys.$checkNull( "es"));};

const enDic =sys.$checkNull( {
  "Accept": "Accept",
  "Activate": "Activate",
  "Adjustment": "Adjustment",
  "Background": "Background",
  "Blur": "Blur",
  "Blur (0 - 100)": "Blur (0 - 100)",
  "Bottom": "Bottom",
  "Cancel": "Cancel",
  "Click %0 to continue.": "Click %0 to continue.",
  "Close": "Close",
  "Color": "Color",
  "Cut": "Cut",
  "Deactivate": "Deactivate",
  "Image '%0' can not be processed": "Image '%0' can not be processed",
  "KtWeb session is closed.\nAuthenticating from KtWeb:Main.": "KtWeb session is closed.\nAuthenticating from KtWeb:Main.",
  "Left": "Left",
  "Light (0 - 100)": "Light (0 - 100)",
  "Percentage (0 - 100)": "Percentage (0 - 100)",
  "Pixels from top / left": "Pixels from top / left",
  "Pixels to sample": "Pixels to sample",
  "Restore": "Restore",
  "Right": "Right",
  "Save in Group %0": "Save in Group %0",
  "Save pictures in group %0?": "Save pictures in group %0?",
  "Session is expired.": "Session is expired.",
  "Stretch": "Stretch",
  "There are duplicated pictures": "There are duplicated pictures",
  "There are no pictures to adjust": "There are no pictures to adjust",
  "Top": "Top",
  "Update": "Update",
  "here": "here"
});

export  function en() {sys.$params(arguments.length, 0); Lang[0] =sys.$checkExists(Lang[0],sys.$checkNull( "en"));};


 function dicByKey(s)  {sys.$params(arguments.length, 1);    
  return sys.$eq(s,"es")? esDic:
  sys.$eq(s,"en")? enDic:
   "Unreachable"
;};

const Lang =sys.$checkNull( ["es"]);

export  function getLang() {sys.$params(arguments.length, 0);  return Lang[0];};


export  function tlt(s)  {sys.$params(arguments.length, 1);
  const T =sys.$checkNull( dic.get(dicByKey(Lang[0]), s));
  return sys.asBool( T) ? T[0] : s;
};


export  function fmt(tp, Rpls)  {sys.$params(arguments.length, 2);
  const R =sys.$checkNull( [tp]);
  for (let i = 0;i < arr.size(Rpls); ++i) R[0] =sys.$checkExists(R[0],sys.$checkNull( str.replace(R[0], "%" + sys.toStr(i), Rpls[i])));
   return R[0];
};
