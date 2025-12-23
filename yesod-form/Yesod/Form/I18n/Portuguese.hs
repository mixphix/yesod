{-# LANGUAGE OverloadedStrings #-}

module Yesod.Form.I18n.Portuguese where

import Data.Text (Text)
import Yesod.Form.Types (FormMessage (..))

portugueseFormMessage :: FormMessage -> Text
portugueseFormMessage (MsgInvalidInteger t) = "Número inteiro inválido: " <> t
portugueseFormMessage (MsgInvalidNumber t) = "Número inválido: " <> t
portugueseFormMessage (MsgInvalidEntry t) = "Entrada inválida: " <> t
portugueseFormMessage MsgInvalidTimeFormat = "Hora inválida, deve estar no formato HH:MM[:SS]"
portugueseFormMessage MsgInvalidDay = "Data inválida, deve estar no formado AAAA-MM-DD"
portugueseFormMessage (MsgInvalidUrl t) = "URL inválida: " <> t
portugueseFormMessage (MsgInvalidEmail t) = "Endereço de e-mail inválido: " <> t
portugueseFormMessage (MsgInvalidHour t) = "Hora inválida: " <> t
portugueseFormMessage (MsgInvalidMinute t) = "Minutos inválidos: " <> t
portugueseFormMessage (MsgInvalidSecond t) = "Segundos inválidos: " <> t
portugueseFormMessage MsgCsrfWarning =
  "Como uma proteção contra ataques CSRF, por favor confirme a submissão do seu formulário."
portugueseFormMessage MsgValueRequired = "Preenchimento obrigatório"
portugueseFormMessage (MsgInputNotFound t) = "Entrada não encontrada: " <> t
portugueseFormMessage MsgSelectNone = "<Nenhum>"
portugueseFormMessage (MsgInvalidBool t) = "Booleano inválido: " <> t
portugueseFormMessage MsgBoolYes = "Sim"
portugueseFormMessage MsgBoolNo = "Não"
portugueseFormMessage MsgDelete = "Remover?"
portugueseFormMessage (MsgInvalidHexColorFormat t) = "Cor inválida, deve estar no formato #rrggbb hexadecimal: " <> t
portugueseFormMessage (MsgInvalidDatetimeFormat t) =
  "Data e hora inválida, deve estar no formato AAAA-MM-DD(T| )HH:MM[:SS]: " <> t
