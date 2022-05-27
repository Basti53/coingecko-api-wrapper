# coingecko-api-wrapper
This is a simple wrapper for the [coingecko api](https://www.coingecko.com/en/api) in haskell. Example of use:
```haskell
ghci> priceNow "bitcoin" "eur"
Right 26989.0
ghci> import Data.Time.Calendar.OrdinalDate
ghci> day = fromOrdinalDate 2017 69
ghci> day
2017-03-10
ghci> price "monero" "usd" day
Right 13.041845958784137
```