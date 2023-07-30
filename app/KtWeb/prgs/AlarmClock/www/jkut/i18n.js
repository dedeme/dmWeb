import * as iter from './_js/iter.js';import * as str from './_js/str.js';import * as bytes from './_js/bytes.js';import * as cryp from './_js/cryp.js';import * as dic from './_js/dic.js';import * as timer from './_js/timer.js';import * as js from './_js/js.js';import * as storage from './_js/storage.js';import * as sys from './_js/sys.js';import * as math from './_js/math.js';import * as domo from './_js/domo.js';import * as ui from './_js/ui.js';import * as arr from './_js/arr.js';import * as time from './_js/time.js';import * as client from './_js/client.js';import * as b64 from './_js/b64.js';



const esDic =sys.$checkNull( {
  "Add": "Añadir",
  "Bad Format. Use": "Mal formato. Use:",
  "Click %0 to continue.": "Click %0 para continuar.",
  "Duplicated alarm": "Alarma ya existente",
  "Hour out of range": "La hora está fuera de rango",
  "KtWeb session is closed.\nAuthenticating from KtWeb:Main.": "La sesión de KtWeb ha sido cerrada.\nHay que autenticarse en KtWeb:Main.",
  "Minutes out of range": "Los minutos están fuera de rango.",
  "New Alarm": "Nueva alarma",
  "Programmed Alarms": "Alarmas programadas",
  "Remove the alarm": "Eliminar la alarma",
  "Separator is missing": "Falta el separador",
  "Session is expired.": "Las sesión ha expirado.",
  "Today": "Hoy",
  "Without Alarms": "Sin alarmas",
  "at": "a las",
  "here": "aquí",
  "or": "o"
});

export  function es() {sys.$params(arguments.length, 0); Lang[0] =sys.$checkExists(Lang[0],sys.$checkNull( "es"));};

const enDic =sys.$checkNull( {
  "Add": "Add",
  "Bad Format. Use": "Bad Format. Use",
  "Click %0 to continue.": "Click %0 to continue.",
  "Duplicated alarm": "Duplicated alarm",
  "Hour out of range": "Hour out of range",
  "KtWeb session is closed.\nAuthenticating from KtWeb:Main.": "KtWeb session is closed.\nAuthenticating from KtWeb:Main.",
  "Minutes out of range": "Minutes out of range",
  "New Alarm": "New Alarm",
  "Programmed Alarms": "Programmed Alarms",
  "Remove the alarm": "Remove the alarm",
  "Separator is missing": "Separator is missing",
  "Session is expired.": "Session is expired.",
  "Today": "Today",
  "Without Alarms": "Without Alarms",
  "at": "at",
  "here": "here",
  "or": "or"
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
