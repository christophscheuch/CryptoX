#' Save orderbook
#' @export

save_orderbook <- function(exchange = "bitfinex",
                           asset_pair = "BTCUSD",
                           level = 5, ob = NA){
  if(is.na(ob)) {
    ob <- get_orderbook(exchange = exchange,
                        asset_pair = asset_pair,
                        level = level)
  }
  print(ob)
  dir.create(file.path(exchange), showWarnings = FALSE)
  save(ob, file = paste0(exchange, "/",
                         asset_pair, "_orderbook_", ob[[1]], ".RData"))
}
