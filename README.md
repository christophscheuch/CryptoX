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
get_orderbook(exchange = "gdax"", asset_pair = "BTCUSD", level = 5)
```

To store orderbook data use

```
store_orderbook(exchange = "gdax", asset_pair = "BTCUSD", level = 5)
```

The resulting file contains a list with the orderbook of the corresponding exchange (BTC/ USD) up to the first 5 levels (can be adjuted using the level parameter). The files are stored in a subfolder named after the exchange.

## Supported Exchanges and Assets

Currently supported exchanges include
```
supported_exchanges <- c("binance", "bitfinex", "bitflyer", "bitmex", "bitstamp",
                         "bittrex", "btcc", "cex", "gdax", "gemini", "hitbtc", 
                         "kraken", "lykke", "poloniex")
```
Currently supported asset pairs include

```
supported_assets <- c("BTCUSD", "ETHUSD", "XRPUSD", "LTCUSD")
```

Note: Tether (USDT) is used if the exchange does not offer USD fiat pairs (e.g. Binance, Bittrex, Poloniex). Below is a list of the currently supported exchanges and whether orderbook data for a specific asset pair is available.

| Exchange      | BTCUSD | ETHUSD | XRPUSD | LTCUSD |
| ------------- |:------:|:------:|:------:|:------:|
| Binance       |   Yes  |   Yes  |   No   |   Yes  |
| Bitfinex      |   Yes  |   Yes  |   Yes  |   Yes  |
| bitFlyer      |   Yes  |   No   |   No   |   No   |
| BitMEX        |   Yes  |   No   |   No   |   No   |
| Bitstamp      |   Yes  |   Yes  |   Yes  |   Yes  |
| Bittrex       |   Yes  |   Yes  |   Yes  |   Yes  |
| BTCC          |   Yes  |   No   |   No   |   Yes  |
| CeX           |   Yes  |   Yes  |   Yes  |   No   |
| Gemini        |   Yes  |   Yes  |   No   |   No   |
| GDAX          |   Yes  |   Yes  |   No   |   Yes  |
| HitBTC        |   Yes  |   Yes  |   No   |   Yes  |
| Kraken        |   Yes  |   Yes  |   Yes  |   Yes  |
| Lykke         |   Yes  |   Yes  |   No   |   No   |
| Poloniex      |   Yes  |   Yes  |   Yes  |   Yes  |
