import * as iter from './_js/iter.js';import * as str from './_js/str.js';import * as bytes from './_js/bytes.js';import * as cryp from './_js/cryp.js';import * as dic from './_js/dic.js';import * as timer from './_js/timer.js';import * as js from './_js/js.js';import * as storage from './_js/storage.js';import * as sys from './_js/sys.js';import * as math from './_js/math.js';import * as domo from './_js/domo.js';import * as ui from './_js/ui.js';import * as arr from './_js/arr.js';import * as time from './_js/time.js';import * as client from './_js/client.js';import * as b64 from './_js/b64.js';



const esDic =sys.$checkNull( {
  "'From' is missing": "No hay valor para el campo 'Desde'",
  "'From' is root": "'Desde' es raíz",
  "'Id' is missing": "No ha valor para el campo 'Id'",
  "'To' is missing": "No hay valor para el campo 'A'",
  "'To' is root": "'A' es raíz",
  "Already exists a backup with id '%0'\nOverwrite?": "Ya existe una copia de seguridad con id '%0'\n¿Sobreescribirla?",
  "Backup finalized": "Copia de seguridad finalizada",
  "Backup of '%0'": "Copia de seguridad de '%0'",
  "Click %0 to continue.": "Click %0 para continuar.",
  "Delete '%0'?": "¿Borrar '%0'?",
  "From": "Desde",
  "Id": "Id",
  "KtWeb session is closed.\nAuthenticating from KtWeb:Main.": "La sesión de KtWeb ha sido cerrada.\nHay que autenticarse en KtWeb:Main.",
  "List": "Lista",
  "Paths": "Rutas",
  "Session is expired.": "Las sesión ha expirado.",
  "Time to read files list is not kown yet": "Tiempo para leer la lista de archivos no finalizado todavía",
  "To": "A",
  "Without Backups": "Sin copias de seguridad",
  "here": "aquí"
});

export  function es() {sys.$params(arguments.length, 0); Lang[0] =sys.$checkExists(Lang[0],sys.$checkNull( "es"));};

const enDic =sys.$checkNull( {
  "'From' is missing": "'From' is missing",
  "'From' is root": "'From' is root",
  "'Id' is missing": "'Id' is missing",
  "'To' is missing": "'To' is missing",
  "'To' is root": "'To' is root",
  "Already exists a backup with id '%0'\nOverwrite?": "Already exists a backup with id '%0'\nOverwrite?",
  "Backup finalized": "Backup finalized",
  "Backup of '%0'": "Backup of '%0'",
  "Click %0 to continue.": "Click %0 to continue.",
  "Delete '%0'?": "Delete '%0'?",
  "From": "From",
  "Id": "Id",
  "KtWeb session is closed.\nAuthenticating from KtWeb:Main.": "KtWeb session is closed.\nAuthenticating from KtWeb:Main.",
  "List": "List",
  "Paths": "Paths",
  "Session is expired.": "Session is expired.",
  "Time to read files list is not kown yet": "Time to read files list is not kown yet",
  "To": "To",
  "Without Backups": "Without Backups",
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
