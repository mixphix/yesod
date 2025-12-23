{-# LANGUAGE OverloadedStrings #-}

module Yesod.Form.I18n.English where

import Data.Text (Text)
import Yesod.Form.Types (FormMessage (..))

englishFormMessage :: FormMessage -> Text
englishFormMessage (MsgInvalidInteger t) = "Invalid integer: " <> t
englishFormMessage (MsgInvalidNumber t) = "Invalid number: " <> t
englishFormMessage (MsgInvalidEntry t) = "Invalid entry: " <> t
englishFormMessage MsgInvalidTimeFormat = "Invalid time, must be in HH:MM[:SS] format"
englishFormMessage MsgInvalidDay = "Invalid day, must be in YYYY-MM-DD format"
englishFormMessage (MsgInvalidUrl t) = "Invalid URL: " <> t
englishFormMessage (MsgInvalidEmail t) = "Invalid e-mail address: " <> t
englishFormMessage (MsgInvalidHour t) = "Invalid hour: " <> t
englishFormMessage (MsgInvalidMinute t) = "Invalid minute: " <> t
englishFormMessage (MsgInvalidSecond t) = "Invalid second: " <> t
englishFormMessage MsgCsrfWarning =
  "As a protection against cross-site request forgery attacks, please confirm your form submission."
englishFormMessage MsgValueRequired = "Value is required"
englishFormMessage (MsgInputNotFound t) = "Input not found: " <> t
englishFormMessage MsgSelectNone = "<None>"
englishFormMessage (MsgInvalidBool t) = "Invalid boolean: " <> t
englishFormMessage MsgBoolYes = "Yes"
englishFormMessage MsgBoolNo = "No"
englishFormMessage MsgDelete = "Delete?"
englishFormMessage (MsgInvalidHexColorFormat t) = "Invalid color, must be in #rrggbb hexadecimal format: " <> t
englishFormMessage (MsgInvalidDatetimeFormat t) = "Invalid datetime, must be in YYYY-MM-DD(T| )HH:MM[:SS] format: " <> t
