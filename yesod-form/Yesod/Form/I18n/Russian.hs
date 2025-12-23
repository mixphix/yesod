{-# LANGUAGE OverloadedStrings #-}

module Yesod.Form.I18n.Russian where

import Data.Text (Text)
import Yesod.Form.Types (FormMessage (..))

russianFormMessage :: FormMessage -> Text
russianFormMessage (MsgInvalidInteger t) = "Неверно записано целое число: " <> t
russianFormMessage (MsgInvalidNumber t) = "Неверный формат числа: " <> t
russianFormMessage (MsgInvalidEntry t) = "Неверный выбор: " <> t
russianFormMessage MsgInvalidTimeFormat = "Неверно указано время, используйте формат ЧЧ:ММ[:СС]"
russianFormMessage MsgInvalidDay = "Неверно указана дата, используйте формат ГГГГ-ММ-ДД"
russianFormMessage (MsgInvalidUrl t) = "Неверно указан URL адрес: " <> t
russianFormMessage (MsgInvalidEmail t) = "Неверно указана электронная почта: " <> t
russianFormMessage (MsgInvalidHour t) = "Неверно указан час: " <> t
russianFormMessage (MsgInvalidMinute t) = "Неверно указаны минуты: " <> t
russianFormMessage (MsgInvalidSecond t) = "Неверно указаны секунды: " <> t
russianFormMessage MsgCsrfWarning =
  "Для защиты от межсайтовой подделки запросов (CSRF), пожалуйста, подтвердите отправку данных формы."
russianFormMessage MsgValueRequired = "Обязательно к заполнению"
russianFormMessage (MsgInputNotFound t) = "Поле не найдено: " <> t
russianFormMessage MsgSelectNone = "<Не выбрано>"
russianFormMessage (MsgInvalidBool t) = "Неверное логическое значение: " <> t
russianFormMessage MsgBoolYes = "Да"
russianFormMessage MsgBoolNo = "Нет"
russianFormMessage MsgDelete = "Удалить?"
russianFormMessage (MsgInvalidHexColorFormat t) =
  "Недопустимое значение цвета, должен быть в шестнадцатеричном формате #rrggbb: "
    <> t
russianFormMessage (MsgInvalidDatetimeFormat t) =
  "Недопустимое значение даты и времени. Должно быть в формате ГГГГ-ММ-ДД(T| )ЧЧ:ММ[:СС]: "
    <> t
