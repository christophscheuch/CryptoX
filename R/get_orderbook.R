#' Get orderbook
#' @description This function scraps data from the orderbook API of a number of exchanges
#' Currently implemented are *bitfinex, coinbase, bitstamp, cex, btcc, binance, bittrex, kraken*, and *bitflyer*
#' @param exchange str name of the exchange (default = "bitfinex")
#' @param asset_pair str name of the asset pair (default = "BTCUSD")
#' @param level Required orderbook level (default = 5, upper bound = 25)
#' @return List with Timestamp (Unix-format), Ask Side (Price and Quantity), Bid (Price and Quantity)
#' @export
#' @importFrom jsonlite fromJSON

get_orderbook <- function(exchange = "bitfinex",
                          asset_pair = "BTCUSD",
                          level = 5){

  if (!exchange %in% supported_exchanges) {
    stop("Exchange does not exist or is currently not supported!")
  }

  if (!asset_pair %in% supported_assets) {
    stop("Asset pair does not exist or is currently not supported!")
  }

  if(exchange == "binance") {
    url <- paste0("https://api.binance.com/api/v1/depth?symbol=",
                  asset_pair, "T&limit=", level)
  }

  if(exchange == "bitfinex") {
    url <- paste0("https://api.bitfinex.com/v1/book/", tolower(asset_pair))
  }

  if(exchange == "bitflyer") {
    if (!asset_pair %in% c("BTCJPY", "BTCUSD", "ETHBTC", "BCHBTC")) {
      stop("bitFlyer does not support this asset pair!")
    }
    url <- paste0("https://api.bitflyer.com/v1/board?product_code=",
                  substr(asset_pair, 1, 3), "_", substr(asset_pair, 4, 6))
  }

  if(exchange == "bitstamp") {
    if (asset_pair != "BTCUSD") {
      stop("Bitstamp only provides order books for BTCUSD!")
    }
    url <- "https://www.bitstamp.net/api/order_book"
  }

  if(exchange == "bittrex") {

    url <- paste0("https://bittrex.com/api/v1.1/public/getorderbook?market=",
                  substr(asset_pair, 4, 6), "T-", substr(asset_pair, 1, 3),
                  "&type=both")
    parsed <- jsonlite::fromJSON(url, simplifyVector = FALSE)
    ask <- t(sapply(parsed$result$buy,
                    function(x) matrix(as.numeric(unlist(x))))[-3, 1:level])
    bid <- t(sapply(parsed$result$sell,
                    function(x) matrix(as.numeric(unlist(x))))[-3, 1:level])
    timestamp <- as.numeric(Sys.time())
    result <- list(exchange = exchange,
                   asset_pair = asset_pair,
                   level = level,
                   timestamp = timestamp,
                   bid = bid,
                   ask = ask)
  }

  if(exchange == "btcc") {
    if (!asset_pair %in% c("BTCUSD")) {
      stop("BTCC only provides order books for BTCUSD!")
    }
    url <- paste0("https://spotusd-data.btcc.com/data/pro/orderbook?symbol=",
                  asset_pair, "&limit=", level)
  }

  if(exchange == "cex") {
    url <- paste0("https://cex.io/api/order_book/",
                  substr(asset_pair, 1, 3), "/", substr(asset_pair, 4, 6))
  }

  if(exchange == "coinbase") {
    url <- paste0("https://api.gdax.com/products/",
                  substr(asset_pair, 1, 3), "-", substr(asset_pair, 4, 6),
                  "/book?level=2")
    }

  if(exchange == "kraken") {
    url <- "https://api.kraken.com/0/public/Depth?pair=XBTUSD"
    parsed <- jsonlite::fromJSON(url, simplifyVector = FALSE)
    timestamp <- as.numeric(Sys.time())
    ask <- t(sapply(parsed$result$XXBTZUSD$asks,
                    function(x) matrix(as.numeric(unlist(x))))[-3, 1:level])
    bid <- t(sapply(parsed$result$XXBTZUSD$bids,
                    function(x) matrix(as.numeric(unlist(x))))[-3, 1:level])
    result <- list(exchange = exchange,
                   asset_pair = asset_pair,
                   level = level,
                   timestamp = timestamp,
                   bid = bid,
                   ask = ask)
  }

  if (exchange == "lykke") {
    url <- paste0("https://hft-api.lykke.com/api/OrderBooks/", asset_pair)
    parsed <- jsonlite::fromJSON(url, simplifyVector = FALSE)
    timestamp <- as.numeric(Sys.time())
    ask <- abs(t(sapply(parsed[[1]]$Prices,
                    function(x) matrix(as.numeric(unlist(x))))[-3, 1:level]))
    ask <- ask[, c(2, 1)]
    bid <- t(sapply(parsed[[2]]$Prices,
                    function(x) matrix(as.numeric(unlist(x))))[-3, 1:level])
    bid <- bid[, c(2, 1)]
    result <- list(exchange = exchange,
                   asset_pair = asset_pair,
                   level = level,
                   timestamp = timestamp,
                   bid = bid,
                   ask = ask)

  }

  if (exchange == "poloniex") {
    url <- paste0("https://poloniex.com/public?command=returnOrderBook&currencyPair=",
                  substr(asset_pair, 4, 6), "T_", substr(asset_pair, 1, 3),
                  "&depth=", level)
  }

  if(!exchange %in% c('kraken', 'bittrex', 'lykke')){

    parsed <- jsonlite::fromJSON(url, simplifyVector = FALSE)

    if (exchange == 'bitfinex') {
      timestamp <- as.numeric(parsed[[1]][[1]]$timestamp)
    }
    if (exchange == 'coinbase') {
      timestamp <- as.numeric(Sys.time())
    }
    if (exchange == 'bitstamp') {
      timestamp <- as.numeric(parsed$timestamp)
    }
    if (exchange == 'cex') {
      timestamp <- as.numeric(parsed$timestamp)
    }
    if (exchange == 'btcc') {
      timestamp <- as.numeric(parsed$date)
    }
    if (exchange == 'binance') {
      timestamp <- as.numeric(Sys.time())
    }
    if (exchange == 'bitflyer') {
      timestamp <- as.numeric(Sys.time())
    }
    if (exchange == "poloniex") {
      timestamp <- as.numeric(Sys.time())
    }

    bid <- t(sapply(parsed$bids,
                    function(x) matrix(as.numeric(unlist(x))))[-3, 1:level])
    ask <- t(sapply(parsed$asks,
                    function(x) matrix(as.numeric(unlist(x))))[-3, 1:level])
    result <- list(exchange = exchange,
                   level = level,
                   asset_pair = asset_pair,
                   timestamp = timestamp,
                   bid = bid,
                   ask = ask)
  }

  return(result)

}
