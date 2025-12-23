{-# LANGUAGE OverloadedStrings #-}

module Yesod.Form.I18n.French (frenchFormMessage) where

import Data.Text (Text)
import Yesod.Form.Types (FormMessage (..))

frenchFormMessage :: FormMessage -> Text
frenchFormMessage (MsgInvalidInteger t) = "Entier invalide : " <> t
frenchFormMessage (MsgInvalidNumber t) = "Nombre invalide : " <> t
frenchFormMessage (MsgInvalidEntry t) = "Entrée invalide : " <> t
frenchFormMessage MsgInvalidTimeFormat = "Heure invalide (elle doit être au format HH:MM ou HH:MM:SS"
frenchFormMessage MsgInvalidDay = "Date invalide (elle doit être au format AAAA-MM-JJ"
frenchFormMessage (MsgInvalidUrl t) = "Adresse Internet invalide : " <> t
frenchFormMessage (MsgInvalidEmail t) = "Adresse électronique invalide : " <> t
frenchFormMessage (MsgInvalidHour t) = "Heure invalide : " <> t
frenchFormMessage (MsgInvalidMinute t) = "Minutes invalides : " <> t
frenchFormMessage (MsgInvalidSecond t) = "Secondes invalides  " <> t
frenchFormMessage MsgCsrfWarning = "Afin d'empêcher les attaques CSRF, veuillez ré-envoyer ce formulaire"
frenchFormMessage MsgValueRequired = "Ce champ est requis"
frenchFormMessage (MsgInputNotFound t) = "Entrée non trouvée : " <> t
frenchFormMessage MsgSelectNone = "<Rien>"
frenchFormMessage (MsgInvalidBool t) = "Booléen invalide : " <> t
frenchFormMessage MsgBoolYes = "Oui"
frenchFormMessage MsgBoolNo = "Non"
frenchFormMessage MsgDelete = "Détruire ?"
frenchFormMessage (MsgInvalidHexColorFormat t) = "Couleur non valide. doit être au format hexadécimal #rrggbb : " <> t
frenchFormMessage (MsgInvalidDatetimeFormat t) =
  "Date/heure non valide. doit être au format AAAA-MM-JJ(T| )HH:MM[:SS] : " <> t
