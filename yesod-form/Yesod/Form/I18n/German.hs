{-# LANGUAGE OverloadedStrings #-}

module Yesod.Form.I18n.German where

import Data.Text (Text)
import Yesod.Form.Types (FormMessage (..))

germanFormMessage :: FormMessage -> Text
germanFormMessage (MsgInvalidInteger t) = "Ungültige Ganzzahl: " <> t
germanFormMessage (MsgInvalidNumber t) = "Ungültige Zahl: " <> t
germanFormMessage (MsgInvalidEntry t) = "Ungültiger Eintrag: " <> t
germanFormMessage MsgInvalidTimeFormat = "Ungültiges Zeitformat, HH:MM[:SS] Format erwartet"
germanFormMessage MsgInvalidDay = "Ungültiges Datum, JJJJ-MM-TT Format erwartet"
germanFormMessage (MsgInvalidUrl t) = "Ungültige URL: " <> t
germanFormMessage (MsgInvalidEmail t) = "Ungültige e-Mail Adresse: " <> t
germanFormMessage (MsgInvalidHour t) = "Ungültige Stunde: " <> t
germanFormMessage (MsgInvalidMinute t) = "Ungültige Minute: " <> t
germanFormMessage (MsgInvalidSecond t) = "Ungültige Sekunde: " <> t
germanFormMessage MsgCsrfWarning =
  "Bitte bestätigen Sie ihre Eingabe, als Schutz gegen Cross-Site Forgery Angriffe"
germanFormMessage MsgValueRequired = "Wert wird benötigt"
germanFormMessage (MsgInputNotFound t) = "Eingabe nicht gefunden: " <> t
germanFormMessage MsgSelectNone = "<Nichts>"
germanFormMessage (MsgInvalidBool t) = "Ungültiger Wahrheitswert: " <> t
germanFormMessage MsgBoolYes = "Ja"
germanFormMessage MsgBoolNo = "Nein"
germanFormMessage MsgDelete = "Löschen?"
germanFormMessage (MsgInvalidHexColorFormat t) = "Ungültige Farbe, muss im Hexadezimalformat #rrggbb vorliegen: " <> t
germanFormMessage (MsgInvalidDatetimeFormat t) =
  "Ungültige Datums- und Uhrzeitangabe, muss im Format YYYY-MM-DD(T| )HH:MM[:SS] vorliegen: "
    <> t
