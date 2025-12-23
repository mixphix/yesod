{-# LANGUAGE OverloadedStrings #-}

module Yesod.Form.I18n.Romanian where

import Data.Text (Text)
import Yesod.Form.Types (FormMessage (..))

-- | Romanian translation
--
-- @since 1.7.5
romanianFormMessage :: FormMessage -> Text
romanianFormMessage (MsgInvalidInteger t) = "Număr întreg nevalid: " <> t
romanianFormMessage (MsgInvalidNumber t) = "Număr nevalid: " <> t
romanianFormMessage (MsgInvalidEntry t) = "Valoare nevalidă: " <> t
romanianFormMessage MsgInvalidTimeFormat = "Oră nevalidă. Formatul necesar este HH:MM[:SS]"
romanianFormMessage MsgInvalidDay = "Dată nevalidă. Formatul necesar este AAAA-LL-ZZ"
romanianFormMessage (MsgInvalidUrl t) = "Adresă URL nevalidă: " <> t
romanianFormMessage (MsgInvalidEmail t) = "Adresă de e-mail nevalidă: " <> t
romanianFormMessage (MsgInvalidHour t) = "Oră nevalidă: " <> t
romanianFormMessage (MsgInvalidMinute t) = "Minut nevalid: " <> t
romanianFormMessage (MsgInvalidSecond t) = "Secundă nevalidă: " <> t
romanianFormMessage MsgCsrfWarning =
  "Ca protecție împotriva atacurilor CSRF, vă rugăm să confirmați trimiterea formularului."
romanianFormMessage MsgValueRequired = "Câmp obligatoriu"
romanianFormMessage (MsgInputNotFound t) = "Valoare inexistentă: " <> t
romanianFormMessage MsgSelectNone = "<Niciuna>"
romanianFormMessage (MsgInvalidBool t) = "Valoare booleană nevalidă: " <> t
romanianFormMessage MsgBoolYes = "Da"
romanianFormMessage MsgBoolNo = "Nu"
romanianFormMessage MsgDelete = "Șterge?"
romanianFormMessage (MsgInvalidHexColorFormat t) = "Culoare nevalidă. Formatul necesar este #rrggbb în hexazecimal: " <> t
romanianFormMessage (MsgInvalidDatetimeFormat t) =
  "Data și ora nevalidă, trebuie să fie în format AAAA-LL-ZZ(T| )HH:MM[:SS]: "
    <> t
