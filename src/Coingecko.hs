{-# LANGUAGE DeriveGeneric #-}
module Coingecko
    ( priceNow
    , price
    ) where

import Data.Time (Day)
import Data.Aeson (FromJSON)
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

priceNow :: String -> String -> IO (Either String Double)
priceNow name currency = do 
    let url = "https://api.coingecko.com/api/v3/coins/" ++ name
    request <- parseRequest url :: IO Request
    response <- httpJSONEither request :: IO (Response (Either JSONException MarketData))
    let body = getResponseBody response
    case body of 
        Left _ -> return $ Left "Not Found"
        Right marketData -> 
            if currency == "eur" 
                then return $ Right $ eur $ current_price $ market_data marketData
            else if currency == "usd"
                then return $ Right $ usd $ current_price $ market_data marketData
            else return $ Left "Currency Not Supported"

price :: String -> String -> Day -> IO (Either String Double)
price _ _ _ = return $ Right 0