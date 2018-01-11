# CryptoX

Simple R wrapper for public APIs of cryptocurrency exchanges to extract orderbook information.

To install the package, run

```
devtools:install_github('ckscheuch/CryptoX')
library('CryptoX')
```
## Usage

`library(CryptoX)` will load the package. 

To get orderbook data, simply type e.g.

```
get_orderbook(exchange = "coinbase"", asset_pair = "BTCUSD", level = 5)
```

To store orderbook data use

```
store_orderbook(exchange = "coinbase", asset_pair = "BTCUSD", level = 5)
```

The resulting file contains a list with the orderbook of the corresponding exchange (BTC/ USD) up to the first 5 levels (can be adjuted using the level parameter). The files are stored in a subfolder named after the exchange.

## Supported Exchanges and Assets

Currently supported exchanges include
```
supported_exchanges <- c("binance", "bitfinex", "bitflyer", "bitstamp",
                         "bittrex", "btcc", "cex", "coinbase", "kraken",
                         "lykke", "poloniex")
```
Currently supported asset pairs include

```
supported_assets <- c("BTCUSD", "ETHUSD")
```

Note that Tether (USDT) is used if the exchange does not offer USD fiat pairs (e.g. Binance, Bittrex, Poloniex).
