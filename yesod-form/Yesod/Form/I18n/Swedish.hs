{-# LANGUAGE OverloadedStrings #-}

module Yesod.Form.I18n.Swedish where

import Data.Text (Text)
import Yesod.Form.Types (FormMessage (..))

swedishFormMessage :: FormMessage -> Text
swedishFormMessage (MsgInvalidInteger t) = "Ogiltigt antal: " <> t
swedishFormMessage (MsgInvalidNumber t) = "Ogiltigt nummer: " <> t
swedishFormMessage (MsgInvalidEntry t) = "Invalid entry: " <> t
swedishFormMessage MsgInvalidTimeFormat = "Ogiltigt klockslag, måste vara på formatet HH:MM[:SS]"
swedishFormMessage MsgInvalidDay = "Ogiltigt datum, måste vara på formatet ÅÅÅÅ-MM-DD"
swedishFormMessage (MsgInvalidUrl t) = "Ogiltig URL: " <> t
swedishFormMessage (MsgInvalidEmail t) = "Ogiltig epostadress: " <> t
swedishFormMessage (MsgInvalidHour t) = "Ogiltig timme: " <> t
swedishFormMessage (MsgInvalidMinute t) = "Ogiltig minut: " <> t
swedishFormMessage (MsgInvalidSecond t) = "Ogiltig sekund: " <> t
swedishFormMessage MsgValueRequired = "Fältet är obligatoriskt"
swedishFormMessage (MsgInputNotFound t) = "Fältet hittades ej: " <> t
swedishFormMessage MsgSelectNone = "<Ingenting>"
swedishFormMessage (MsgInvalidBool t) = "Ogiltig boolesk: " <> t
swedishFormMessage MsgBoolYes = "Ja"
swedishFormMessage MsgBoolNo = "Nej"
swedishFormMessage MsgDelete = "Radera?"
swedishFormMessage MsgCsrfWarning =
  "Som skydd mot \"cross-site request forgery\" attacker, vänligen bekräfta skickandet av formuläret."
swedishFormMessage (MsgInvalidHexColorFormat t) = "Ogiltig färg, måste vara i #rrggbb hexadecimalt format: " <> t
swedishFormMessage (MsgInvalidDatetimeFormat t) =
  "Ogiltig datumtid, måste vara i formatet ÅÅÅÅ-MM-DD(T| )TT:MM[:SS]: "
    <> t
