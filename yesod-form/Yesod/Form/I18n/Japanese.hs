{-# LANGUAGE OverloadedStrings #-}

module Yesod.Form.I18n.Japanese where

import Data.Text (Text)
import Yesod.Form.Types (FormMessage (..))

japaneseFormMessage :: FormMessage -> Text
japaneseFormMessage (MsgInvalidInteger t) = "無効な整数です: " <> t
japaneseFormMessage (MsgInvalidNumber t) = "無効な数値です: " <> t
japaneseFormMessage (MsgInvalidEntry t) = "無効な入力です: " <> t
japaneseFormMessage MsgInvalidTimeFormat = "無効な時刻です。HH:MM[:SS]フォーマットで入力してください"
japaneseFormMessage MsgInvalidDay = "無効な日付です。YYYY-MM-DDフォーマットで入力してください"
japaneseFormMessage (MsgInvalidUrl t) = "無効なURLです: " <> t
japaneseFormMessage (MsgInvalidEmail t) = "無効なメールアドレスです: " <> t
japaneseFormMessage (MsgInvalidHour t) = "無効な時間です: " <> t
japaneseFormMessage (MsgInvalidMinute t) = "無効な分です: " <> t
japaneseFormMessage (MsgInvalidSecond t) = "無効な秒です: " <> t
japaneseFormMessage MsgCsrfWarning = "CSRF攻撃を防ぐため、フォームの入力を確認してください"
japaneseFormMessage MsgValueRequired = "値は必須です"
japaneseFormMessage (MsgInputNotFound t) = "入力が見つかりません: " <> t
japaneseFormMessage MsgSelectNone = "<なし>"
japaneseFormMessage (MsgInvalidBool t) = "無効なbool値です: " <> t
japaneseFormMessage MsgBoolYes = "はい"
japaneseFormMessage MsgBoolNo = "いいえ"
japaneseFormMessage MsgDelete = "削除しますか?"
japaneseFormMessage (MsgInvalidHexColorFormat t) = "無効な色。＃rrggbb16進形式である必要があります: " <> t
japaneseFormMessage (MsgInvalidDatetimeFormat t) = "無効な日時です。YYYY-MM-DD(T| )HH:MM[:SS] 形式である必要があります: " <> t
