{-# LANGUAGE OverloadedStrings #-}

module Yesod.Form.I18n.Spanish where

import Data.Text (Text)
import Yesod.Form.Types (FormMessage (..))

spanishFormMessage :: FormMessage -> Text
spanishFormMessage (MsgInvalidInteger t) = "Número entero inválido: " <> t
spanishFormMessage (MsgInvalidNumber t) = "Número inválido: " <> t
spanishFormMessage (MsgInvalidEntry t) = "Entrada inválida: " <> t
spanishFormMessage MsgInvalidTimeFormat = "Hora inválida, debe tener el formato HH:MM[:SS]"
spanishFormMessage MsgInvalidDay = "Fecha inválida, debe tener el formato AAAA-MM-DD"
spanishFormMessage (MsgInvalidUrl t) = "URL inválida: " <> t
spanishFormMessage (MsgInvalidEmail t) = "Dirección de correo electrónico inválida: " <> t
spanishFormMessage (MsgInvalidHour t) = "Hora inválida: " <> t
spanishFormMessage (MsgInvalidMinute t) = "Minuto inválido: " <> t
spanishFormMessage (MsgInvalidSecond t) = "Segundo inválido: " <> t
spanishFormMessage MsgCsrfWarning = "Como protección contra ataques CSRF, confirme su envío por favor."
spanishFormMessage MsgValueRequired = "Se requiere un valor"
spanishFormMessage (MsgInputNotFound t) = "Entrada no encontrada: " <> t
spanishFormMessage MsgSelectNone = "<Ninguno>"
spanishFormMessage (MsgInvalidBool t) = "Booleano inválido: " <> t
spanishFormMessage MsgBoolYes = "Sí"
spanishFormMessage MsgBoolNo = "No"
spanishFormMessage MsgDelete = "¿Eliminar?"
spanishFormMessage (MsgInvalidHexColorFormat t) = "Color no válido, debe estar en formato hexadecimal #rrggbb: " <> t
spanishFormMessage (MsgInvalidDatetimeFormat t) =
  "Fecha y hora no válida; debe estar en formato AAAA-MM-DD(T| )HH:MM[:SS]: "
    <> t
