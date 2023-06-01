import * as iter from './_js/iter.js';import * as str from './_js/str.js';import * as bytes from './_js/bytes.js';import * as cryp from './_js/cryp.js';import * as dic from './_js/dic.js';import * as timer from './_js/timer.js';import * as js from './_js/js.js';import * as storage from './_js/storage.js';import * as sys from './_js/sys.js';import * as math from './_js/math.js';import * as domo from './_js/domo.js';import * as ui from './_js/ui.js';import * as arr from './_js/arr.js';import * as time from './_js/time.js';import * as client from './_js/client.js';import * as b64 from './_js/b64.js';



const esDic =sys.$checkNull( {
  "2 Days": "2 días",
  "Accept": "Aceptar",
  "All": "Todo",
  "All log entries will be deleted.\nContinue?": "Todas las entradas serán borradas.\n¿Continuar?",
  "Application exit?": "¿Terminar la aplicación?",
  "Cancel": "Cancelar",
  "Change Language to %0": "Cambiar el lenguaje a %0",
  "Change Password": "Cambiar la contraseña",
  "Check gray squares": "Marcar las casillas grises",
  "Click %0 to continue.": "Click %0 para continuar.",
  "Confirm password is missing": "Falta la confirmación de la contraseña",
  "Current password": "Contraseña actual",
  "Current password is missing": "Falta la contraseña actual.",
  "Delete": "Borrar",
  "Errors": "Errores",
  "Grey squares checks are wrong": "Las casillas grises está mal marcadas",
  "Home": "Inicio",
  "Keep connected": "Mantenerse conectado",
  "Log": "Registro",
  "Login": "Identificación",
  "Logout-message": "<p>%0 ha terminado.</p><p><b>¡Hasta pronto!</b></p>",
  "New password": "Nueva contraseña",
  "New password and confirm password do not match": "La contraseña y su confirmación no coinciden",
  "New password is missing": "Falta la nueva contraseña.",
  "Password": "Contraseña",
  "Password is missing": "Falta la contraseña",
  "Password successfully changed": "Contaseña correctamente cambiada.",
  "Reload": "Recargar",
  "Session is expired.": "Las sesión ha expirado.",
  "Settings": "Configuración",
  "User": "Usuario",
  "User name is missing": "Falta el nombre del usuario",
  "Wrong password": "Contraseña incorrecta",
  "here": "aquí"
});

export  function es() {sys.$params(arguments.length, 0); Lang[0] =sys.$checkExists(Lang[0],sys.$checkNull( "es"));};

const enDic =sys.$checkNull( {
  "2 Days": "2 Days",
  "Accept": "Accept",
  "All": "All",
  "All log entries will be deleted.\nContinue?": "All log entries will be deleted.\nContinue?",
  "Application exit?": "Application exit?",
  "Cancel": "Cancel",
  "Change Language to %0": "Change Language to %0",
  "Change Password": "Change Password",
  "Check gray squares": "Check gray squares",
  "Click %0 to continue.": "Click %0 to continue.",
  "Confirm password is missing": "Confirm password is missing",
  "Current password": "Current password",
  "Current password is missing": "Current password is missing",
  "Delete": "Delete",
  "Errors": "Errors",
  "Grey squares checks are wrong": "Grey squares checks are wrong",
  "Home": "Home",
  "Keep connected": "Keep connected",
  "Log": "Log",
  "Login": "Login",
  "Logout-message": "<p>%0 has finished.</p><p><b>Good by!</b></p>",
  "New password": "New password",
  "New password and confirm password do not match": "New password and confirm password do not match",
  "New password is missing": "New password is missing",
  "Password": "Password",
  "Password is missing": "Password is missing",
  "Password successfully changed": "Password successfully changed",
  "Reload": "Reload",
  "Session is expired.": "Session is expired.",
  "Settings": "Settings",
  "User": "User",
  "User name is missing": "User name is missing",
  "Wrong password": "Wrong password",
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
