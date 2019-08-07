#' Get symbols
#' @description This function scraps the symbols from the public API of a host of exchanges
#' @param exchange Name of an exchange (e.g. "binance", "kraken", "lykke")
#' @param asset_filter Name of asset or asset pair to filter out of all symbols (e.g. EUR, BTC, ETHUSD)
#' @return Character vector with symbols tradeable on specific exchange
#' @export
#' @importFrom jsonlite fromJSON

get_symbols <- function(exchange = as.character(NA), asset_filter = as.character(NA)) {

  if (is.na(exchange)) {
    stop("Exchange not specified!")
  }

  if (!exchange %in% c("binance", "bitfinex", "bitflyer", "bitmex", "bitpanda", "bitstamp",
                       "bittrex", "bl3p", "cex", "coinbasepro", "exmo", "gate",
                       "gemini", "hitbtc", "kraken", "latoken", "lykke", "okcoin",
                       "paymium", "poloniex", "therocktrading", "xbtce")) {
    stop("Exchange does not exist or is currently not supported!")
  }

  if (exchange == "binance") {
    url <- "https://api.binance.com/api/v1/ticker/allPrices"
    parsed <- jsonlite::fromJSON(url, simplifyVector = FALSE)
    symbols <- sort(as.character(unlist(sapply(parsed,
                                          "[", "symbol"))))
  }

  if (exchange == "bitfinex") {
    url <- "https://api.bitfinex.com/v1/symbols"
    parsed <- jsonlite::fromJSON(url, simplifyVector = FALSE)
    symbols <- sort(toupper(as.character(unlist(parsed))))
  }

  if (exchange == "bitflyer") {
    url1 <- "https://api.bitflyer.com/v1/getmarkets/usa"
    parsed1 <- jsonlite::fromJSON(url1, simplifyVector = FALSE)
    url2 <- "https://api.bitflyer.com/v1/getmarkets/eu"
    parsed2 <- jsonlite::fromJSON(url2, simplifyVector = FALSE)
    symbols <- c(sort(gsub("_", "", as.character(unlist(sapply(parsed1,
                                                               "[", "product_code"))))),
                 sort(gsub("_", "", as.character(unlist(sapply(parsed2,
                                                               "[", "product_code"))))))
  }

  if (exchange == "bitmex") {
    symbols <- c("BTCUSD")
  }

  if (exchange == "bitpanda") {
    url <- "https://api.exchange.bitpanda.com/public/v1/instruments"
    parsed <- jsonlite::fromJSON(url, simplifyVector = FALSE)
    base <- unlist(sapply(sapply(parsed, "[", "base"), "[", "code"))
    quote <- unlist(sapply(sapply(parsed, "[", "quote"), "[", "code"))
    symbols <- sort(paste0(base, quote))
  }

  if (exchange == "bitstamp") {
    url <- "https://www.bitstamp.net/api/v2/trading-pairs-info/"
    parsed <- jsonlite::fromJSON(url, simplifyVector = FALSE)
    symbols <- sort(gsub("/", "",
                         as.character(unlist(sapply(parsed, "[", "name")))))
  }

  if (exchange == "bittrex") {
    url <- "https://bittrex.com/api/v1.1/public/getmarkets"
    parsed <- jsonlite::fromJSON(url, simplifyVector = FALSE)
    symbols <- sort(paste0(as.character(unlist(sapply(parsed$result,
                                                 "[", "MarketCurrency"))),
                      as.character(unlist(sapply(parsed$result,
                                                 "[", "BaseCurrency")))))
  }

  if (exchange == "bl3p") {
    symbols <- "BTCEUR"
  }

  if (exchange == "cex") {
    url <- "https://cex.io/api/currency_limits"
    parsed <- jsonlite::fromJSON(url, simplifyVector = FALSE)
    symbols <- sort(paste0(as.character(unlist(sapply(parsed$data$pairs,
                                                 "[", "symbol1"))),
                      as.character(unlist(sapply(parsed$data$pairs,
                                                 "[", "symbol2")))))
  }

  if (exchange == "gate") {
    url <- "http://data.gate.io/api2/1/pairs"
    parsed <- jsonlite::fromJSON(url, simplifyVector = FALSE)
    symbols <- sort(gsub("_", "", unlist(toupper(parsed))))
  }

  if (exchange == "coinbasepro") {
    url <- "https://api.pro.coinbase.com/products/"
    parsed <- jsonlite::fromJSON(url, simplifyVector = FALSE)
    symbols <- sort(gsub("-", "", as.character(unlist(sapply(parsed,
                                                             "[", "id")))))
  }

  if (exchange == "exmo") {
    url <- "https://api.exmo.com/v1/ticker/"
    parsed <- jsonlite::fromJSON(url, simplifyVector = FALSE)
    symbols <- sort(gsub("_", "", names(parsed)))
  }

  if(exchange == "gemini") {
    url <- "https://api.gemini.com/v1/symbols"
    parsed <- jsonlite::fromJSON(url, simplifyVector = FALSE)
    symbols <- sort(toupper(unlist(parsed)))
  }

  if (exchange == "hitbtc") {
    url <- "https://api.hitbtc.com/api/2/public/symbol"
    parsed <- jsonlite::fromJSON(url, simplifyVector = FALSE)
    symbols <- sort(as.character(unlist(sapply(parsed, "[", "id"))))
  }

  if (exchange == "kraken") {
    url <- "https://api.kraken.com/0/public/AssetPairs"
    parsed <- jsonlite::fromJSON(url, simplifyVector = FALSE)
    symbols <- sort(as.character(unlist(sapply(parsed$result,
                                               "[", "altname"))))
  }

  if (exchange == "latoken") {
    url <- "https://api.latoken.com/api/v1/MarketData/ticker"
    parsed <- jsonlite::fromJSON(url, simplifyVector = FALSE)
    symbols <- sort(as.character(unlist(sapply(parsed, "[", "symbol"))))
  }

  if (exchange == "lykke") {
    url <- "https://hft-api.lykke.com/api/AssetPairs"
    parsed <- jsonlite::fromJSON(url, simplifyVector = FALSE)
    symbols <- sort(as.character(unlist(sapply(parsed, "[", "Id"))))
  }

  if (exchange == "okcoin") {
    url <- "https://www.okcoin.com/api/spot/v3/instruments/ticker"
    parsed <- jsonlite::fromJSON(url, simplifyVector = FALSE)
    symbols <- sort(as.character(unlist(sapply(parsed, "[", "product_id"))))
    symbols <- gsub("-", "", symbols)
  }

  if (exchange == "paymium") {
    symbols <- "BTCEUR"
  }

  if (exchange == "poloniex") {
    url <- "https://poloniex.com/public?command=returnTicker"
    parsed <- jsonlite::fromJSON(url, simplifyVector = FALSE)
    symbols <- sort(gsub("_", "", names(parsed)))
  }

  if (exchange == "therocktrading") {
    url <- "https://api.therocktrading.com/v1/funds/tickers"
    parsed <- jsonlite::fromJSON(url, simplifyVector = FALSE)
    symbols <- sort(as.character(unlist(sapply(parsed$tickers, "[", "fund_id"))))
  }

  if (exchange == "xbtce") {
    url <- "https://cryptottlivewebapi.xbtce.net:8443/api/v1/public/symbol"
    parsed <- jsonlite::fromJSON(url, simplifyVector = FALSE)
    symbols <- sort(as.character(unlist(sapply(parsed, "[", "Symbol"))))
  }

  # filter out relevant asset pairs
  if (!is.na(asset_filter)) {
    symbols <- symbols[grepl(paste0(asset_filter), symbols)]
  }

  return(symbols)

}
