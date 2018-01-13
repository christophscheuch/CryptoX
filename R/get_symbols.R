#' Get symbols
#' @description This function scraps the symbols from the public API of a host of exchanges
#' @param exchange str name of the exchange
#' @return List with symbols tradeable on specific exchange
#' @export
#' @importFrom jsonlite fromJSON

get_symbols <- function(exchange = as.character(NA)) {

  if (is.na(exchange)) {
    stop("Exchange not specified!")
  }

  if (!exchange %in% supported_exchanges) {
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
    url <- "https://api.bitflyer.com/v1/getmarkets/usa"
    parsed <- jsonlite::fromJSON(url, simplifyVector = FALSE)
    symbols <- sort(gsub("_", "", as.character(unlist(sapply(parsed,
                                          "[", "product_code")))))
  }

  if (exchange == "bitmex") {
    symbols = "BTCUSD"
  }

  if (exchange == "bitstamp") {
    url <- "https://www.bitstamp.net/api/v2/trading-pairs-info/"
    parsed <- jsonlite::fromJSON(url, simplifyVector = FALSE)
    symbols <- sort(gsub("/", "",
                         as.character(unlist(sapply(parsed, "[", "name")))))
  }

  if (exchange == "bttrex") {
    url <- "https://bittrex.com/api/v1.1/public/getmarkets"
    parsed <- jsonlite::fromJSON(url, simplifyVector = FALSE)
    symbols <- sort(paste0(as.character(unlist(sapply(parsed$result,
                                                 "[", "MarketCurrency"))),
                      as.character(unlist(sapply(parsed$result,
                                                 "[", "BaseCurrency")))))
  }

  if (exchange == "btcc") {
    symbols <- "BTCUSD"
  }

  if (exchange == "cex") {
    url <- "https://cex.io/api/currency_limits"
    parsed <- jsonlite::fromJSON(url, simplifyVector = FALSE)
    symbols <- sort(paste0(as.character(unlist(sapply(parsed$data$pairs,
                                                 "[", "symbol1"))),
                      as.character(unlist(sapply(parsed$data$pairs,
                                                 "[", "symbol2")))))
  }

  if (exchange == "gdax") {
    url <- "https://api.gdax.com/products/"
    parsed <- jsonlite::fromJSON(url, simplifyVector = FALSE)
    symbols <- sort(gsub("-", "", as.character(unlist(sapply(parsed,
                                                             "[", "id")))))
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
    stop("Kraken down...")
  }

  if (exchange == "lykke") {
    url <- "https://hft-api.lykke.com/api/AssetPairs"
    parsed <- jsonlite::fromJSON(url, simplifyVector = FALSE)
    symbols <- sort(as.character(unlist(sapply(parsed, "[", "Id"))))
  }

  if (exchange == "poloniex") {
    url <- "https://poloniex.com/public?command=returnTicker"
    parsed <- jsonlite::fromJSON(url, simplifyVector = FALSE)
    symbols <- sort(gsub("_", "", names(parsed)))
  }

  return(symbols)

}
