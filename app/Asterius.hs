{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
module Asterius where

import Flags
import Converter
import qualified Data.Text as T
import Control.Monad ((>=>))

import Data.Aeson
import Asterius.Types
import Asterius.Aeson

import GHC.Generics

-- Conversion requests, responses, and their errablility
data Errable a = Errable (Either String a)

data Request = Request
    { input :: String
    , keepNumerals :: Bool
    , newline :: Bool
    }
    deriving (Show, Generic)

data Response = Response
    { pronounciation :: String
    , int :: String
    }
    deriving (Show, Generic)

instance ToJSON Request
instance ToJSON Response
instance FromJSON Response
instance FromJSON Request

instance ToJSON a => ToJSON (Errable a) where
    toJSON (Errable (Left errMsg)) =
        object [ T.pack "type" .= "error", T.pack "data" .= errMsg ]
    toJSON (Errable (Right result)) =
        object [ T.pack "type" .= "success", T.pack "data" .= result ]

--
foreign export javascript "testAeson" testAeson :: JSVal -> JSVal
foreign export javascript "tryConwayWechsler" tryConwayWechslerJS :: JSVal -> JSVal

-- Handle parsing, error wrapping, and calling to/fromJSVal
aesonE2E :: (FromJSON a, ToJSON b) => (a -> Either String b) -> JSVal -> JSVal
aesonE2E f jsval = jsonToJSVal $ Errable $ jsonFromJSVal jsval >>= f

-- Test function for debugging
testAeson :: JSVal -> JSVal
testAeson = aesonE2E $ \req ->
    if keepNumerals req || newline req
       then Left "one of keepNumerals or newline set"
       else Right $ Response { pronounciation = "neither keepNumerals nor newline set", int = "1000" }


-- Perform Conway-Wechsler conversion
tryConwayWechslerJS :: JSVal -> JSVal
tryConwayWechslerJS = aesonE2E tryConwayWechsler

tryConwayWechsler :: Request -> Either String Response
tryConwayWechsler req = do
    expr <- tryStrToExpr (input req)
    int <- tryExprToInt True expr
    let flags = [KeepNumerals | keepNumerals req] ++ [Newline | newline req]
    let pronounciation = convert flags int
    pure $ Response
        { pronounciation = T.unpack pronounciation
        , int = show int
        }
