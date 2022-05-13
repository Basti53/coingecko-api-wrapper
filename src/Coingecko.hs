{-# LANGUAGE DeriveGeneric #-}
module Coingecko
    ( priceNow
    , price
    ) where

import Data.Time 
import Data.Aeson (FromJSON)
import GHC.Generics (Generic)
import Network.HTTP.Client.Conduit (Request, parseRequest)
import Network.HTTP.Simple (Response, getResponseBody, httpJSON)

data Currencies = Currencies {
    eur :: Double 
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

priceNow :: String -> IO (Either String Double)
priceNow name = do 
    let url = "https://api.coingecko.com/api/v3/coins/" ++ name
    request <- parseRequest url :: IO Request
    response <- httpJSON request :: IO (Response MarketData)
    return $ Right $ eur $ current_price $ market_data $ getResponseBody response

price :: String -> Day -> IO (Either String Double)
price _ _ = return $ Right 0