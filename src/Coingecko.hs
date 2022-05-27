{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Coingecko
    ( priceNow
    , price
    ) where

import Data.Time
import Data.Aeson (FromJSON, parseJSON, withObject, (.:))
import GHC.Generics (Generic)
import Network.HTTP.Client.Conduit (Request, parseRequest)
import Network.HTTP.Simple (Response, getResponseBody, httpJSONEither, JSONException)

data Currencies = Currencies {
    eur :: Double,
    usd :: Double 
} deriving (Generic, Show)

instance FromJSON Currencies

data CurrentPrice = CurrentPrice {
    current_price :: Currencies
} deriving (Generic, Show)

instance FromJSON CurrentPrice

data MarketData = MarketData {
    market_data :: CurrentPrice
} deriving (Generic, Show)

instance FromJSON MarketData

data Error = Error {
    error' :: String 
} deriving Show

instance FromJSON Error where 
    parseJSON = withObject "Error" $ \v -> Error <$> v .: "error"

priceNow :: String -> String -> IO (Either String Double)
priceNow name currency = do 
    let url = "https://api.coingecko.com/api/v3/coins/" ++ name
    request <- parseRequest url :: IO Request
    response <- httpJSONEither request :: IO (Response (Either JSONException MarketData))
    case getResponseBody response of 
        Left _ -> do 
            response <- httpJSONEither request :: IO (Response (Either JSONException Error))
            case getResponseBody response of 
                Left _ -> return $ Left "Unknown error."
                Right err -> return $ Left $ error' err
        Right marketData -> 
            if currency == "eur" 
                then return $ Right $ eur $ current_price $ market_data marketData
            else if currency == "usd"
                then return $ Right $ usd $ current_price $ market_data marketData
            else return $ Left $ "The currency '" ++ currency ++ "' is not supported."

data Simple = Simple {
    id :: String 
} deriving (Generic, Show)

instance FromJSON Simple 

price :: String -> String -> Day -> IO (Either String Double)
price name currency day = do 
    let url = "https://api.coingecko.com/api/v3/coins/" 
            ++ name ++ "/history?date=" 
            ++ formatTime defaultTimeLocale "%d-%m-%Y" day
    request <- parseRequest url :: IO Request
    response <- httpJSONEither request :: IO (Response (Either JSONException MarketData))
    case getResponseBody response of 
        Left _ -> do 
            response <- httpJSONEither request :: IO (Response (Either JSONException Simple))
            case getResponseBody response of 
                Left _ -> do 
                    response <- httpJSONEither request :: IO (Response (Either JSONException Error))
                    case getResponseBody response of 
                        Left _ -> return $ Left "Unknown error."
                        Right err -> return $ Left $ error' err
                Right _ -> return $ Left "It seems like the price is not available for this date."
        Right marketData -> 
            if currency == "eur" 
                then return $ Right $ eur $ current_price $ market_data marketData
            else if currency == "usd"
                then return $ Right $ usd $ current_price $ market_data marketData
            else return $ Left $ "The currency '" ++ currency ++ "' is not supported."
