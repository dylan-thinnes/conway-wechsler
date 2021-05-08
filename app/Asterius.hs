{-# LANGUAGE LambdaCase #-}
module Asterius where

import Flags
import Converter
import qualified Data.Text as T
import Control.Monad ((>=>))

import Data.Aeson
import Asterius.Types
import Asterius.Aeson

foreign export javascript "tryStrToInt" tryStrToIntJS :: JSString -> JSVal
foreign export javascript "tryStrToPronounciation" tryStrToPronounciationJS :: JSString -> JSVal

tryStrToInt :: Bool -> String -> Either String Integer
tryStrToInt safe = tryStrToExpr >=> tryExprToInt safe

tryStrToIntJS :: JSString -> JSVal
tryStrToIntJS jsstr =
    let inp = fromJSString jsstr
        safe = True
        errRes = tryStrToInt safe inp
        (type_, msg) = case errRes of
            Left err -> ("error", err)
            Right int -> ("success", show int)
    in
    jsonToJSVal $ object [ T.pack "type" .= type_, T.pack "data" .= msg ]

tryStrToPronounciationJS :: JSString -> JSVal
tryStrToPronounciationJS jsstr =
    let inp = fromJSString jsstr
        safe = True
        errRes = convert [] <$> tryStrToInt safe inp
        (type_, msg) = case errRes of
            Left err -> ("error", err)
            Right pronounciation -> ("success", T.unpack pronounciation)
    in
    jsonToJSVal $ object [ T.pack "type" .= type_, T.pack "data" .= msg ]
