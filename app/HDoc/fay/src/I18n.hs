-- Copyright 25-Feb-20 ºDeme
-- GNU General Public License - V3 <http://www.gnu.org/licenses/>
 
--- Internationalization utilies.
 
module I18n
  ( i18nInit
  , __
  , i18nFormat
  ) where
 
import Fay.Unsafe
import qualified Data.Var as Var
import qualified Data.Text as Text
 
lang :: Var.Ref (Maybe String)
lang = unsafePerformFay $ Var.newRef Nothing
 
_es :: [(String, String)]
_es =
  [ ("here", "aquí")
  , ("Click %0 to continue", "Para continuar pulse %0.")
  , ("New password and confirm password do not match", "La nueva contraseña y su confirmación no coinciden.")
  , ("Confirm password is missing", "Falta la confirmación de la contraseña")
  , ("New password is missing", "Falta por indicar la nueva contraseña")
  , ("Current password is missing", "Falta por indicar la contraseña actual")
  , ("Grey squares checks are wrong", "Las casillas grises están mal marcadas")
  , ("Password successfully changed", "La contraseña se cambió adecuadamente")
  , ("Check gray squares", "Marcar los cuadrados grises")
  , ("Wrong password", "La contraseña es incorrecta")
  , ("Accept", "Aceptar")
  , ("Cancel", "Cancelar")
  , ("New password", "Nueva contraseña")
  , ("New password", "Nueva contraseña")
  , ("Current password", "Contraseña actual")
  , ("Password Change", "Cambio de contraseña")
  , ("Delete '%0'?", "¿Eliminar '%0'?")
  , ("Change Password", "Cambiar la contraseña")
  , ("Change Language to %0", "Change Language to %0")
  , ("Path", "Ruta")
  , ("Name", "Nombre")
  , ("Libraries", "Librerías")
  , ("There are no libraries", "No hay librerías")
  , ("Path is missing", "Falta la ruta")
  , ("Name contains '%0'", "El nombre contiene '%0'")
  , ("Name is duplicate", "El nombre está repetido")
  , ("Name is missing", "Falta el nombre")
  , ("Logout-message", "<p>%0 ha terminado.</p><p><b>¡Hasta pronto!</b></p>")
  , ("Session is expired.", "La sesión ha expirado.")
  , ("Password is missing", "Falta por indicar la contraseña")
  , ("User name is missing", "Falta por indicar el nombre del usuario")
  , ("Grey squares checks are wrong", "Las casillas grises están mal marcadas")
  , ("Check gray squares", "Marcar los cuadrados grises")
  , ("Wrong password", "La contraseña es incorrecta")
  , ("Accept", "Aceptar")
  , ("Keep connected", "Mantenerse conectado")
  , ("Password", "Contraseña")
  , ("User", "Usuario")
  , ("Login", "Identificación")
  ]
_en :: [(String, String)]
_en =
  [ ("here", "here")
  , ("Click %0 to continue", "Click %0 to continue.")
  , ("New password and confirm password do not match", "New password and confirm password do not match")
  , ("Confirm password is missing", "'Confirm password' is missing")
  , ("New password is missing", "'New password' is missing")
  , ("Current password is missing", "'Current password' is missing")
  , ("Grey squares checks are wrong", "Grey squares checks are wrong")
  , ("Password successfully changed", "Password successfully changed")
  , ("Check gray squares", "Check gray squares")
  , ("Wrong password", "Wrong password")
  , ("Accept", "Accept")
  , ("Cancel", "Cancel")
  , ("New password", "New password")
  , ("New password", "New password")
  , ("Current password", "Current password")
  , ("Password Change", "Password Change")
  , ("Delete '%0'?", "Delete '%0'?")
  , ("Change Password", "Change Password")
  , ("Change Language to %0", "Cambiar el idioma a %0")
  , ("Path", "Path")
  , ("Name", "Name")
  , ("Libraries", "Libraries")
  , ("There are no libraries", "There are no libraries")
  , ("Path is missing", "Path is missing")
  , ("Name contains '%0'", "Name contains '%0'")
  , ("Name is duplicate", "Name is duplicate")
  , ("Name is missing", "Name is missing")
  , ("Logout-message", "<p>%0 has finished.</p><p><b>Good by!</b></p>")
  , ("Session is expired.", "Session is expired.")
  , ("Password is missing", "Password is missing")
  , ("User name is missing", "User name is missing")
  , ("Grey squares checks are wrong", "Grey squares checks are wrong")
  , ("Check gray squares", "Check gray squares")
  , ("Wrong password", "Wrong password")
  , ("Accept", "Accept")
  , ("Keep connected", "Keep connected")
  , ("Password", "Password")
  , ("User", "User")
  , ("Login", "Login")
  ]

get :: [(String, String)] -> String -> String
get ls s = case lookup s ls of Just v -> v; _ -> s
 
--- i18nInit lg
--- Set the application language. Only can be called once.
i18nInit :: String -> Fay ()
i18nInit lg = do
  l <- Var.get lang
  case l of
    Nothing -> Var.set lang (Just lg)
    _ -> fail "Duplicate 'i18nInit' call"
 
--- __ text
--- Makes text translation to the application language.
---   text  : String literal. It has the following properties.
---           - Multiline and blank strings are not allowed
---           - Characters bellow blank (control characters) are removed.
---   return: A String retrieved from a dictionary.
---           Tranlated text is transcribed as it is. For example
---           = this is\n"a thing" is transcribed as "this is\n\"a thing\""
__ :: String -> String
__ k = unsafePerformFay $ do
  l <- Var.get lang
  case l of
    Just lg -> case lg of
                 "es" -> return $ get _es k
                 "en" -> return $ get _en k
                 _ -> fail $ "'" ++ lg ++ "' unknown language"
    _ -> return k 
--- format lang params
--- Utility to replace variables (similar to C printf).
--- Example:
---   'i18nFormat "%2 is %1%%" ["50", "1/2"]' produce '1/2 is 50%'
i18nFormat :: String -> [String] -> String
i18nFormat tpl params = let (r, _) = foldl replaceN (tpl, 0::Int) params
                      in  replace r "%%" "%"
  where
    replace :: String -> String -> String -> String
    replace s sub rep = Text.unpack $ Text.intercalate (Text.pack rep) $
                          Text.splitOn (Text.pack sub) (Text.pack s)
    replaceN (t, n) s = (replace t ("%" ++ (show n)) s, n + 1)
