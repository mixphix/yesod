{-# LANGUAGE OverloadedStrings #-}

module Yesod.Form.I18n.Czech where

import Data.Text (Text)
import Yesod.Form.Types (FormMessage (..))

czechFormMessage :: FormMessage -> Text
czechFormMessage (MsgInvalidInteger t) = "Neplatné celé číslo: " <> t
czechFormMessage (MsgInvalidNumber t) = "Neplatné číslo: " <> t
czechFormMessage (MsgInvalidEntry t) = "Neplatná položka: " <> t
czechFormMessage MsgInvalidTimeFormat = "Neplatný čas, musí být ve formátu HH:MM[:SS]"
czechFormMessage MsgInvalidDay = "Neplatný den, musí být formátu YYYY-MM-DD"
czechFormMessage (MsgInvalidUrl t) = "Neplatná URL: " <> t
czechFormMessage (MsgInvalidEmail t) = "Neplatná e-mailová adresa: " <> t
czechFormMessage (MsgInvalidHour t) = "Neplatná hodina: " <> t
czechFormMessage (MsgInvalidMinute t) = "Neplatná minuta: " <> t
czechFormMessage (MsgInvalidSecond t) = "Neplatná sekunda: " <> t
czechFormMessage MsgCsrfWarning =
  "Prosím potvrďte odeslání formuláře jako ochranu před útekem „cross-site request forgery“."
czechFormMessage MsgValueRequired = "Hodnota je vyžadována"
czechFormMessage (MsgInputNotFound t) = "Vstup nebyl nalezen: " <> t
czechFormMessage MsgSelectNone = "<Nic>"
czechFormMessage (MsgInvalidBool t) = "Neplatná pravdivostní hodnota: " <> t
czechFormMessage MsgBoolYes = "Ano"
czechFormMessage MsgBoolNo = "Ne"
czechFormMessage MsgDelete = "Smazat?"
czechFormMessage (MsgInvalidHexColorFormat t) = "Neplatná barva, musí být v #rrggbb hexadecimálním formátu: " <> t
czechFormMessage (MsgInvalidDatetimeFormat t) = "Neplatné datum a čas, musí být ve formátu YYYY-MM-DD(T| )HH:MM[:SS]: " <> t
