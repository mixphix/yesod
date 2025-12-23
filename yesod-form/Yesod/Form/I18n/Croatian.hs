{-# LANGUAGE OverloadedStrings #-}

module Yesod.Form.I18n.Croatian where

import Yesod.Form.Types (FormMessage (..))
import Data.Text (Text)

croatianFormMessage :: FormMessage -> Text
croatianFormMessage (MsgInvalidInteger t) = "Cjelobrojna vrijednost nije valjana: " <> t
croatianFormMessage (MsgInvalidNumber t)  = "Broj nije valjan: " <> t
croatianFormMessage (MsgInvalidEntry t)   = "Unos nije valjan: " <> t
croatianFormMessage MsgInvalidTimeFormat  = "Vrijeme nije valjano, mora biti u obliku HH:MM[:SS]"
croatianFormMessage MsgInvalidDay         = "Dan nije valjan, mora biti u obliku GGGG-MM-DD"
croatianFormMessage (MsgInvalidUrl t)     = "URL adresa nije valjana: " <> t
croatianFormMessage (MsgInvalidEmail t)   = "Adresa e-pošte nije valjana: " <> t
croatianFormMessage (MsgInvalidHour t)    = "Sat nije valjan: " <> t
croatianFormMessage (MsgInvalidMinute t)  = "Minuta nije valjana: " <> t
croatianFormMessage (MsgInvalidSecond t)  = "Sekunda nije valjana: " <> t
croatianFormMessage MsgCsrfWarning        = "Potvrdite slanje obrasca radi zaštite od XSRF napada,"
croatianFormMessage MsgValueRequired      = "Potrebno je unijeti vrijednost"
croatianFormMessage (MsgInputNotFound t)  = "Unos nije pronađen: " <> t
croatianFormMessage MsgSelectNone         = "<nema>"
croatianFormMessage (MsgInvalidBool t)    = "Logička vrijednost nije valjana: " <> t
croatianFormMessage MsgBoolYes            = "Da"
croatianFormMessage MsgBoolNo             = "Ne"
croatianFormMessage MsgDelete             = "Izbrisati?"
croatianFormMessage (MsgInvalidHexColorFormat t) = "Nevažeća boja, mora biti u #rrggbb heksadecimalnom formatu: " <> t
croatianFormMessage (MsgInvalidDatetimeFormat t) = "Nevažeći datum i vrijeme, mora biti u formatu GGGG-MM-DD(T| )HH:MM[:SS]: " <> t
