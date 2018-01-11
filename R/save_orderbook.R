#' Save orderbook
#' @param exchange str name of the exchange
#' @param asset_pair str name of the asset pair
#' @param level Required orderbook level (default = 5, upper bound = 25)
#' @export

save_orderbook <- function(exchange = as.character(NA),
                           asset_pair = as.character(NA),
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
