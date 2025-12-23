{-# LANGUAGE OverloadedStrings #-}

module Yesod.Form.I18n.Chinese where

import Data.Text (Text)
import Yesod.Form.Types (FormMessage (..))

chineseFormMessage :: FormMessage -> Text
chineseFormMessage (MsgInvalidInteger t) = "无效的整数: " <> t
chineseFormMessage (MsgInvalidNumber t) = "无效的数字: " <> t
chineseFormMessage (MsgInvalidEntry t) = "无效的条目: " <> t
chineseFormMessage MsgInvalidTimeFormat = "无效的时间, 必须符合HH:MM[:SS]格式"
chineseFormMessage MsgInvalidDay = "无效的日期, 必须符合YYYY-MM-DD格式"
chineseFormMessage (MsgInvalidUrl t) = "无效的链接: " <> t
chineseFormMessage (MsgInvalidEmail t) = "无效的邮箱地址: " <> t
chineseFormMessage (MsgInvalidHour t) = "无效的小时: " <> t
chineseFormMessage (MsgInvalidMinute t) = "无效的分钟: " <> t
chineseFormMessage (MsgInvalidSecond t) = "无效的秒: " <> t
chineseFormMessage MsgCsrfWarning = "为了防备跨站请求伪造, 请确认表格提交."
chineseFormMessage MsgValueRequired = "此项必填"
chineseFormMessage (MsgInputNotFound t) = "输入找不到: " <> t
chineseFormMessage MsgSelectNone = "<空>"
chineseFormMessage (MsgInvalidBool t) = "无效的逻辑值: " <> t
chineseFormMessage MsgBoolYes = "是"
chineseFormMessage MsgBoolNo = "否"
chineseFormMessage MsgDelete = "删除?"
chineseFormMessage (MsgInvalidHexColorFormat t) = "颜色无效，必须为 #rrggbb 十六进制格式: " <> t
chineseFormMessage (MsgInvalidDatetimeFormat t) = "日期時間無效，必須採用 YYYY-MM-DD(T| )HH:MM[:SS] 格式： " <> t
