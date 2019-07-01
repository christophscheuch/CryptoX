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
get_orderbook(exchange = "gdax", asset_pair = "BTCUSD", level = 10)
```

To store a list that contains orderbook data use
```
save_orderbook(exchange = "binance", asset_pair = "BTCUSD", level = 10)
```

The resulting file contains a list with the orderbook of the corresponding exchange (BTC/ USD) up to the first 5 levels (can be adjuted using the level parameter). The files are stored in a subfolder named after the exchange.

## Supported Exchanges and Assets

Currently supported exchanges include
```
supported_exchanges <- c("binance", "bitfinex", "bitflyer", "bitmex", "bitstamp",
                         "bittrex", "cex", "gate", "coinbasepro", 
                         "gemini", "hitbtc", "kraken", "lykke", 
                         "poloniex", "xbtce")
```
Currently supported asset pairs include

```
supported_assets <- c("BTCUSD", "BTCEUR", "ETHUSD", "ETHEUR", 
                      "LTCUSD", "LTCEUR", "XRPUSD", "XRPEUR")
```

Below is a list of the currently supported exchanges and whether orderbook data for a specific asset pair is available.

| Exchange                                  | BTCUSD | BTCEUR | ETHUSD | ETHEUR | LTCUSD | LTCEUR | XRPUSD | XRPEUR |
| ----------------------------------------- |:------:|:------:|:------:|:------:|:------:|:------:|:------:|:------:|
| [Binance](https://www.binance.com/)       |   Yes* |   No   |  Yes*  |   No   |  Yes*  |   No   |  Yes*  |   No   |
| [Bitfinex](https://www.bitfinex.com/)     |   Yes  |  Yes   |  Yes   |  Yes   |  Yes   |   No   |  Yes   |   No   |
| [bitFlyer](https://bitflyer.jp/)          |   Yes  |  Yes   |   No   |   No   |   No   |   No   |   No   |   No   |
| [BitMEX](https://www.bitmex.com/)**       |   Yes  |   No   |   No   |   No   |   No   |   No   |   No   |   No   |
| [Bitstamp](https://www.bitstamp.net/)     |   Yes  |  Yes   |   Yes  |  Yes   |  Yes   |  Yes   |  Yes   |  Yes   |
| [Bittrex](https://bittrex.com/)           |   Yes  |   No   |   Yes  |   No   |  Yes   |   No   |  Yes   |   No   |
| [CEX.IO](https://cex.io/)                 |   Yes  |  Yes   |  Yes   |  Yes   |  Yes   |   No   |  Yes   |  Yes   |
| [Coinbase Pro](https://pro.coinbase.com/) |   Yes  |   Yes  |  Yes   |  Yes   |  Yes   |  Yes   |  Yes   |  Yes   |
| [Gate.io](https://gate.io/)               |   Yes  |   No   |  Yes   |   No   |  Yes   |   No   |  Yes   |   No   |
| [Gemini](https://gemini.com/)             |   Yes  |   No   |  Yes   |   No   |  Yes   |   No   |   No   |   No   |
| [HitBTC](https://hitbtc.com/)             |   Yes  |   No   |  Yes   |   No   |  Yes   |   No   |   No   |   No   |
| [Kraken](https://www.kraken.com/)         |   Yes  |  Yes   |  Yes   |  Yes   |  Yes   |  Yes   |  Yes   |  Yes   |
| [Lykke](https://www.lykke.com/)           |   Yes  |  Yes   |  Yes   |  Yes   |  Yes   |   No   |  Yes   |  Yes   |
| [Poloniex](https://poloniex.com/)         |   Yes* |   No   |  Yes*  |   No   |  Yes*  |   No   |  Yes*  |   No   |
| [xBTCe](https://www.xbtce.com/)           |   Yes  |  Yes   |  Yes   |  Yes   |  Yes   |  Yes   |  No    |   No   |

*Note: Tether (USDT) is used if the exchange does not offer USD fiat pairs.

**Note that BitMEX is a derivatives market and not actually spot trading BTCUSD. 
