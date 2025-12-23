{-# LANGUAGE OverloadedStrings #-}

module Yesod.Form.I18n.Dutch where

import Data.Text (Text)
import Yesod.Form.Types (FormMessage (..))

dutchFormMessage :: FormMessage -> Text
dutchFormMessage (MsgInvalidInteger t) = "Ongeldig aantal: " <> t
dutchFormMessage (MsgInvalidNumber t) = "Ongeldig getal: " <> t
dutchFormMessage (MsgInvalidEntry t) = "Ongeldige invoer: " <> t
dutchFormMessage MsgInvalidTimeFormat = "Ongeldige tijd, het juiste formaat is (UU:MM[:SS])"
dutchFormMessage MsgInvalidDay = "Ongeldige datum, het juiste formaat is (JJJJ-MM-DD)"
dutchFormMessage (MsgInvalidUrl t) = "Ongeldige URL: " <> t
dutchFormMessage (MsgInvalidEmail t) = "Ongeldig e-mail adres: " <> t
dutchFormMessage (MsgInvalidHour t) = "Ongeldig uur: " <> t
dutchFormMessage (MsgInvalidMinute t) = "Ongeldige minuut: " <> t
dutchFormMessage (MsgInvalidSecond t) = "Ongeldige seconde: " <> t
dutchFormMessage MsgCsrfWarning =
  "Bevestig het indienen van het formulier, dit als veiligheidsmaatregel tegen \"cross-site request forgery\" aanvallen."
dutchFormMessage MsgValueRequired = "Verplicht veld"
dutchFormMessage (MsgInputNotFound t) = "Geen invoer gevonden: " <> t
dutchFormMessage MsgSelectNone = "<Geen>"
dutchFormMessage (MsgInvalidBool t) = "Ongeldige waarheidswaarde: " <> t
dutchFormMessage MsgBoolYes = "Ja"
dutchFormMessage MsgBoolNo = "Nee"
dutchFormMessage MsgDelete = "Verwijderen?"
dutchFormMessage (MsgInvalidHexColorFormat t) = "Ongeldige kleur, moet de hexadecimale indeling #rrggbb hebben: " <> t
dutchFormMessage (MsgInvalidDatetimeFormat t) =
  "Ongeldige datum/tijd, moet de indeling JJJJ-MM-DD(T| )UU:MM[:SS] hebben: " <> t
