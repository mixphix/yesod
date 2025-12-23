{-# LANGUAGE OverloadedStrings #-}

module Yesod.Form.I18n.Norwegian where

import Data.Text (Text)
import Yesod.Form.Types (FormMessage (..))

norwegianBokmålFormMessage :: FormMessage -> Text
norwegianBokmålFormMessage (MsgInvalidInteger t) = "Ugyldig antall: " <> t
norwegianBokmålFormMessage (MsgInvalidNumber t) = "Ugyldig nummer: " <> t
norwegianBokmålFormMessage (MsgInvalidEntry t) = "Ugyldig oppføring: " <> t
norwegianBokmålFormMessage MsgInvalidTimeFormat = "Ugyldig klokkeslett, må være i formatet HH:MM[:SS]"
norwegianBokmålFormMessage MsgInvalidDay = "Ugyldig dato, må være i formatet ÅÅÅÅ-MM-DD"
norwegianBokmålFormMessage (MsgInvalidUrl t) = "Ugyldig URL: " <> t
norwegianBokmålFormMessage (MsgInvalidEmail t) = "Ugyldig e-postadresse: " <> t
norwegianBokmålFormMessage (MsgInvalidHour t) = "Ugyldig time: " <> t
norwegianBokmålFormMessage (MsgInvalidMinute t) = "Ugyldig minutt: " <> t
norwegianBokmålFormMessage (MsgInvalidSecond t) = "Ugyldig sekund: " <> t
norwegianBokmålFormMessage MsgValueRequired = "Feltet er obligatorisk"
norwegianBokmålFormMessage (MsgInputNotFound t) = "Feltet ble ikke funnet: " <> t
norwegianBokmålFormMessage MsgSelectNone = "<Ingenting>"
norwegianBokmålFormMessage (MsgInvalidBool t) = "Ugyldig sannhetsverdi: " <> t
norwegianBokmålFormMessage MsgBoolYes = "Ja"
norwegianBokmålFormMessage MsgBoolNo = "Nei"
norwegianBokmålFormMessage MsgDelete = "Slette?"
norwegianBokmålFormMessage MsgCsrfWarning =
  "Som beskyttelse mot «cross-site request forgery»-angrep, vennligst bekreft innsendt skjema."
norwegianBokmålFormMessage (MsgInvalidHexColorFormat t) = "Ugyldig farge, må være i #rrggbb heksadesimalt format: " <> t
norwegianBokmålFormMessage (MsgInvalidDatetimeFormat t) = "Ugyldig datoklokkeslett, må være i formatet ÅÅÅÅ-MM-DD(T| )HH:MM[:SS]:" <> t
