# CryptoX

Simple R wrapper for public APIs of cryptocurrency exchanges to extract orderbook information.

To install the package, run

```
devtools::install_github("ckscheuch/CryptoX")
library("CryptoX")
```
## Usage

To get the symbols tradeable on an exhange, type
```
get_symbols("lykke")
```

To get orderbook data, simply type
```
get_orderbook(exchange = "gdax", asset_pair = "BTCUSD", level = 5)
```

To store a list that contains orderbook data use
```
store_orderbook(exchange = "binance", asset_pair = "BTCUSD", level = 5)
```

The resulting file contains a list with the orderbook of the corresponding exchange (BTC/ USD) up to the first 5 levels (can be adjuted using the level parameter). The files are stored in a subfolder named after the exchange.

## Supported Exchanges and Assets

Currently supported exchanges include
```
supported_exchanges <- c("binance", "bitfinex", "bitflyer", "bitmex", "bitstamp",
                         "bittrex", "btcc", "cex", "gate", "gatecoin", "gdax", 
                         "gemini", "hitbtc", "kraken", "liqui", "lykke", 
                         "poloniex", "xbtce")
```
Currently supported asset pairs include

```
supported_assets <- c("BTCUSD", "ETHUSD", "XRPUSD", "LTCUSD")
```

Note: Tether (USDT) is used if the exchange does not offer USD fiat pairs (e.g. Binance, Bittrex, Liqui, Poloniex). Below is a list of the currently supported exchanges and whether orderbook data for a specific asset pair is available.

| Exchange                              | BTCUSD | ETHUSD | XRPUSD | LTCUSD |
| ------------------------------------- |:------:|:------:|:------:|:------:|
| [Binance](https://www.binance.com/)   |   Yes  |   Yes  |   No   |   Yes  |
| [Bitfinex](https://www.bitfinex.com/) |   Yes  |   Yes  |   Yes  |   Yes  |
| [bitFlyer](https://bitflyer.jp/)      |   Yes  |   No   |   No   |   No   |
| [BitMEX](https://www.bitmex.com/)     |   Yes  |   No   |   No   |   No   |
| [Bitstamp](https://www.bitstamp.net/) |   Yes  |   Yes  |   Yes  |   Yes  |
| [Bittrex](https://bittrex.com/)       |   Yes  |   Yes  |   Yes  |   Yes  |
| [BTCC](https://www.btcc.com/)         |   Yes  |   No   |   No   |   No   |
| [CeX](https://cex.io/)                |   Yes  |   Yes  |   Yes  |   No   |
| [Gate](https://gate.io/)              |   Yes  |   Yes  |   Yes  |   Yes  |
| [Gatecoin](https://gatecoin.com/)     |   Yes  |   Yes  |   No   |   Yes  |
| [Gemini](https://gemini.com/)         |   Yes  |   Yes  |   No   |   No   |
| [GDAX](https://www.gdax.com/)         |   Yes  |   Yes  |   No   |   Yes  |
| [HitBTC](https://hitbtc.com/)         |   Yes  |   Yes  |   No   |   Yes  |
| [Kraken](https://www.kraken.com/)     |   Yes  |   Yes  |   Yes  |   Yes  |
| [Liqui](https://liqui.io/)            |   Yes  |   Yes  |   No   |   Yes  |
| [Lykke](https://www.lykke.com/)       |   Yes  |   Yes  |   No   |   No   |
| [Poloniex](https://poloniex.com/)     |   Yes  |   Yes  |   Yes  |   Yes  |
| [xBTCe](https://www.xbtce.com/)       |   Yes  |   Yes  |   Yes  |   Yes  |
