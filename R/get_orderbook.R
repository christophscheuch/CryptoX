#' Get orderbook
#' @description This function scraps orderbook data from the public API of a host of exchanges
#' @param exchange Name of an exchange (e.g. "binance", "kraken", "lykke")
#' @param asset_pair Name of an asset pair (e.g. "BTCUSD", "ETHUSD")
#' @param level Required orderbook level (default = 10, upper bound = 25)
#' @return List with exhange name, asset pair, timestamp (Unix-format), ask side (price and quantity), bid (price and quantity)
#' @export
#' @importFrom jsonlite fromJSON

get_orderbook <- function(exchange = as.character(NA),
                          asset_pair = as.character(NA),
                          level = 10){

  if (is.na(exchange) | is.na(asset_pair)) {
    stop("Exchange / asset pair not specified!")
  }

  if (!exchange %in% supported_exchanges) {
    stop("Exchange does not exist or is currently not supported!")
  }

  if (!asset_pair %in% supported_assets) {
    stop("Asset pair does not exist or is currently not supported!")
  }

  if(exchange == "binance") {
    url <- paste0("https://api.binance.com/api/v1/depth?symbol=",
                  asset_pair, "T")
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

  if (exchange == "bitmex") {
    if (asset_pair == "BTCUSD") {
      url <- paste0("https://www.bitmex.com/api/v1/orderBook/L2?symbol=XBT&depth=", level)
    }
    timestamp <- as.numeric(Sys.time())
    parsed <- jsonlite::fromJSON(url, simplifyVector = FALSE)
    timestamp_received <- as.numeric(Sys.time())
    p <- matrix(unlist(parsed), 5, length(parsed))
    ask <- apply(t(p[c(4, 5), p[3, ] == "Sell"]), 2, as.numeric)
    ask <- ask[, c(2, 1)]
    ask[, 2] <- ask[, 2] / ask[, 1]
    bid <- apply(t(p[c(4, 5), p[3, ] == "Buy"]), 2, as.numeric)
    bid <- bid[, c(2, 1)]
    bid[, 2] <- bid[, 2] / bid[, 1]

    if (nrow(bid) < level) {
      bid <- rbind(bid,
                   matrix(rep(as.numeric(NA), (level - nrow(bid)) * 2), ncol = 2))
    }
    if (nrow(ask) < level) {
      ask <- rbind(ask,
                   matrix(rep(as.numeric(NA), (level - nrow(ask)) * 2), ncol = 2))
    }

    result <- list(exchange = exchange,
                   asset_pair = asset_pair,
                   level = level,
                   timestamp = timestamp,
                   timestamp_received = timestamp_received,
                   timestamp_exchange = as.numeric(NA),
                   bid = bid,
                   ask = ask)
  }

  if(exchange == "bitstamp") {
    url <- paste0("https://www.bitstamp.net/api/v2/order_book/",
                  tolower(asset_pair))
  }

  if(exchange == "bittrex") {

    url <- paste0("https://bittrex.com/api/v1.1/public/getorderbook?market=",
                  substr(asset_pair, 4, 6), "T-", substr(asset_pair, 1, 3),
                  "&type=both")
    timestamp <- as.numeric(Sys.time())
    parsed <- jsonlite::fromJSON(url, simplifyVector = FALSE)
    timestamp_received <- as.numeric(Sys.time())
    ask <- t(sapply(parsed$result$sell,
                    function(x) matrix(as.numeric(unlist(x))))[-3, ])
    ask <- ask[, c(2, 1)]
    ask <- ask[1:(min(nrow(ask), level)), ]
    bid <- t(sapply(parsed$result$buy,
                    function(x) matrix(as.numeric(unlist(x))))[-3, ])
    bid <- bid[, c(2, 1)]
    bid <- bid[1:(min(nrow(bid), level)), ]

    if (nrow(bid) < level) {
      bid <- rbind(bid,
                   matrix(rep(as.numeric(NA), (level - nrow(bid)) * 2), ncol = 2))
    }
    if (nrow(ask) < level) {
      ask <- rbind(ask,
                   matrix(rep(as.numeric(NA), (level - nrow(ask)) * 2), ncol = 2))
    }

    result <- list(exchange = exchange,
                   asset_pair = asset_pair,
                   level = level,
                   timestamp = timestamp,
                   timestamp_received = timestamp_received,
                   timestamp_exchange = as.numeric(NA),
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

  if (exchange == "gate") {
    url <- paste0("http://data.gate.io/api2/1/orderBook/",
                  substr(asset_pair, 1, 3), "_", substr(asset_pair, 4, 6), "t")
  }

  if (exchange == "gatecoin") {
    url <- paste0("https://api.gatecoin.com/Public/MarketDepth/", asset_pair)
  }

  if (exchange == "gdax") {
    url <- paste0("https://api.gdax.com/products/",
                  substr(asset_pair, 1, 3), "-", substr(asset_pair, 4, 6),
                  "/book?level=2")
  }

  if(exchange == "gemini") {
    url <- paste0("https://api.gemini.com/v1/book/", asset_pair)
  }

  if (exchange == "hitbtc") {
    url <- paste0("https://api.hitbtc.com/api/2/public/orderbook/", asset_pair)
    timestamp <- as.numeric(Sys.time())
    parsed <- jsonlite::fromJSON(url, simplifyVector = FALSE)
    timestamp_received <- as.numeric(Sys.time())
    bid <- t(sapply(parsed$bid,
                    function(x) matrix(as.numeric(unlist(x))))[-3, ])
    bid <- bid[1:(min(nrow(bid), level)), ]
    ask <- t(sapply(parsed$ask,
                    function(x) matrix(as.numeric(unlist(x))))[-3, ])
    ask <- ask[1:(min(nrow(ask), level)), ]

    if (nrow(bid) < level) {
      bid <- rbind(bid,
                   matrix(rep(as.numeric(NA), (level - nrow(bid)) * 2), ncol = 2))
    }
    if (nrow(ask) < level) {
      ask <- rbind(ask,
                   matrix(rep(as.numeric(NA), (level - nrow(ask)) * 2), ncol = 2))
    }

    result <- list(exchange = exchange,
                   level = level,
                   asset_pair = asset_pair,
                   timestamp = timestamp,
                   timestamp_received = timestamp_received,
                   timestamp_exchange = as.numeric(NA),
                   bid = bid,
                   ask = ask)
  }

  if(exchange == "kraken") {
    asset_pair <- gsub("BTC", "XBT", asset_pair) #kraken uses XBT ticker for BTC
    url <- paste0("https://api.kraken.com/0/public/Depth?pair=", asset_pair)
    timestamp <- as.numeric(Sys.time())
    parsed <- jsonlite::fromJSON(url, simplifyVector = FALSE)
    timestamp_received <- as.numeric(Sys.time())
    ask <- t(sapply(parsed$result[[1]]$asks,
                    function(x) matrix(as.numeric(unlist(x))))[-3, ])
    ask <- ask[1:(min(nrow(ask), level)), ]
    bid <- t(sapply(parsed$result[[1]]$bids,
                    function(x) matrix(as.numeric(unlist(x))))[-3, ])
    bid <- bid[1:(min(nrow(bid), level)), ]

    if (nrow(bid) < level) {
      bid <- rbind(bid,
                   matrix(rep(as.numeric(NA), (level - nrow(bid)) * 2), ncol = 2))
    }
    if (nrow(ask) < level) {
      ask <- rbind(ask,
                   matrix(rep(as.numeric(NA), (level - nrow(ask)) * 2), ncol = 2))
    }

    result <- list(exchange = exchange,
                   asset_pair = asset_pair,
                   level = level,
                   timestamp = timestamp,
                   timestamp_received = timestamp_received,
                   timestamp_exchange = as.numeric(NA),
                   bid = bid,
                   ask = ask)
  }

  if (exchange == "liqui") {
    url <- paste0("https://api.liqui.io/api/3/depth/",
                  tolower(substr(asset_pair, 1, 3)), "_",
                  tolower(substr(asset_pair, 4, 6)), "t",
                  "?limit=", level)
    timestamp_received <- as.numeric(Sys.time())
    parsed <- jsonlite::fromJSON(url, simplifyVector = FALSE)
    timestamp <- as.numeric(Sys.time())
    bid <- t(sapply(parsed[[1]]$bids,
                    function(x) matrix(as.numeric(unlist(x))))[-3, ])
    ask <- t(sapply(parsed[[1]]$asks,
                    function(x) matrix(as.numeric(unlist(x))))[-3, ])

    if (nrow(bid) < level) {
      bid <- rbind(bid,
                   matrix(rep(as.numeric(NA), (level - nrow(bid)) * 2), ncol = 2))
    }
    if (nrow(ask) < level) {
      ask <- rbind(ask,
                   matrix(rep(as.numeric(NA), (level - nrow(ask)) * 2), ncol = 2))
    }

    result <- list(exchange = exchange,
                   level = level,
                   asset_pair = asset_pair,
                   timestamp = timestamp,
                   timestamp_received = timestamp_received,
                   timestamp_exchange = as.numeric(NA),
                   bid = bid,
                   ask = ask)
  }

  if (exchange == "lykke") {
    url <- paste0("https://hft-api.lykke.com/api/OrderBooks/", asset_pair)
    timestamp <- as.numeric(Sys.time())
    parsed <- jsonlite::fromJSON(url, simplifyVector = FALSE)
    timestamp_received <- as.numeric(Sys.time())
    timestamp_exchange <- as.numeric(as.POSIXct(parsed[[1]]$Timestamp,
                                                format = "%Y-%m-%dT  %H:%M:%OS"))
    ask <- abs(t(sapply(parsed[[1]]$Prices,
                    function(x) matrix(as.numeric(unlist(x))))[-3, ]))
    ask <- ask[, c(2, 1)]
    ask <- ask[1:(min(nrow(ask), level)), ]
    bid <- t(sapply(parsed[[2]]$Prices,
                    function(x) matrix(as.numeric(unlist(x))))[-3, ])
    bid <- bid[, c(2, 1)]
    bid <- bid[1:(min(nrow(bid), level)), ]

    if (nrow(bid) < level) {
      bid <- rbind(bid,
                   matrix(rep(as.numeric(NA), (level - nrow(bid)) * 2), ncol = 2))
    }
    if (nrow(ask) < level) {
      ask <- rbind(ask,
                   matrix(rep(as.numeric(NA), (level - nrow(ask)) * 2), ncol = 2))
    }

    result <- list(exchange = exchange,
                   asset_pair = asset_pair,
                   level = level,
                   timestamp = timestamp,
                   timestamp_received = timestamp_received,
                   timestamp_exchange = timestamp_exchange,
                   bid = bid,
                   ask = ask)

  }

  if (exchange == "poloniex") {
    url <- paste0("https://poloniex.com/public?command=returnOrderBook&currencyPair=",
                  substr(asset_pair, 4, 6), "T_", substr(asset_pair, 1, 3),
                  "&depth=", level)
  }

  if (exchange == "xbtce") {
    url <- paste0("https://cryptottlivewebapi.xbtce.net:8443/api/v1/public/level2/",
                  asset_pair)
    timestamp <- as.numeric(Sys.time())
    parsed <- jsonlite::fromJSON(url, simplifyVector = FALSE)
    timestamp_received <- as.numeric(Sys.time())
    timestamp_exchange <- parsed[[1]]$Timestamp / 1000
    ask <- t(sapply(parsed[[1]]$Asks,
                        function(x) matrix(as.numeric(unlist(x[x != "Asks"]))))[-3, ])
    ask <- as.matrix(t(ask[, c(2, 1)]))
    ask <- as.matrix(t(ask[1:(min(nrow(ask), level)), ]))
    bid <- t(sapply(parsed[[1]]$Bids,
                        function(x) matrix(as.numeric(unlist(x[x != "Bids"]))))[-3, ])
    bid <- as.matrix(t(bid[, c(2, 1)]))
    bid <- as.matrix(t(bid[1:(min(nrow(bid), level)), ]))

    if (nrow(bid) < level) {
      bid <- rbind(bid,
                   matrix(rep(as.numeric(NA), (level - nrow(bid)) * 2), ncol = 2))
    }
    if (nrow(ask) < level) {
      ask <- rbind(ask,
                   matrix(rep(as.numeric(NA), (level - nrow(ask)) * 2), ncol = 2))
    }

    result <- list(exchange = exchange,
                   asset_pair = asset_pair,
                   level = level,
                   timestamp = timestamp,
                   timestamp_received = timestamp_received,
                   timestamp_exchange = timestamp_exchange,
                   bid = bid,
                   ask = ask)
  }

  if(!exchange %in% c("kraken", "bittrex", "liqui", "lykke", "hitbtc", "bitmex",
                      "xbtce")){

    timestamp <- as.numeric(Sys.time())
    parsed <- jsonlite::fromJSON(url, simplifyVector = FALSE)
    timestamp_received <- as.numeric(Sys.time())

    if (exchange == 'bitfinex') {
      timestamp_exchange <- as.numeric(parsed[[1]][[1]]$timestamp)
    }
    if (exchange == "gate") {
      timestamp_exchange <- as.numeric(NA)
    }
    if (exchange == "gatecoin") {
      timestamp_exchange <- as.numeric(NA)
    }
    if (exchange == 'gdax') {
      timestamp_exchange <- as.numeric(NA)
    }
    if (exchange == 'gemini') {
      timestamp_exchange <- as.numeric(NA)
    }
    if (exchange == 'bitstamp') {
      timestamp_exchange <- as.numeric(parsed$timestamp)
    }
    if (exchange == 'cex') {
      timestamp_exchange <- as.numeric(parsed$timestamp)
    }
    if (exchange == 'btcc') {
      timestamp_exchange <- as.numeric(parsed$date) / 1000
    }
    if (exchange == 'binance') {
      timestamp_exchange <- as.numeric(NA)
    }
    if (exchange == 'bitflyer') {
      timestamp_exchange <- as.numeric(NA)
    }
    if (exchange == "poloniex") {
      timestamp_exchange <- as.numeric(NA)
    }

    bid <- t(sapply(parsed$bids,
                  function(x) matrix(as.numeric(unlist(x))))[-3, ])
    bid <- bid[1:(min(nrow(bid), level)), ]
    ask <- t(sapply(parsed$asks,
                    function(x) matrix(as.numeric(unlist(x))))[-3, ])
    ask <- ask[1:(min(nrow(ask), level)), ]

    if (nrow(bid) < level) {
      bid <- rbind(bid,
                    matrix(rep(as.numeric(NA), (level - nrow(bid)) * 2), ncol = 2))
    }
    if (nrow(ask) < level) {
      ask <- rbind(ask,
                   matrix(rep(as.numeric(NA), (level - nrow(ask)) * 2), ncol = 2))
    }

    result <- list(exchange = exchange,
                   level = level,
                   asset_pair = asset_pair,
                   timestamp = timestamp,
                   timestamp_received = timestamp_received,
                   timestamp_exchange = timestamp_exchange,
                   bid = bid,
                   ask = ask)
  }

  return(result)

}
