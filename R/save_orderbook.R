#' Save orderbook
#' @description Saves orderbook to local folder
#' @param exchange Name of an exchange (e.g. "binance", "kraken", "lykke")
#' @param asset_pair Name of an asset pair (e.g. "BTCUSD", "ETHUSD")
#' @param level Required orderbook level (default = 10, upper bound = 25)
#' @param filetype Save as *.rds or *.RData
#' @param print.output Should the output be printed to the console (default = FALSE)?
#' @export

save_orderbook <- function(exchange = as.character(NA),
                           asset_pair = as.character(NA),
                           level = 10, ob = NA,
                           filetype = "rds",
                           print.output = FALSE){
  if(is.na(ob)) {
    ob <- get_orderbook(exchange = exchange,
                        asset_pair = asset_pair,
                        level = level)
  }
  if(print.output) print(ob)
  dir.create(file.path(exchange), showWarnings = FALSE)

  if (filetype == "rds") {
    saveRDS(ob, file = paste0(exchange, "/",
                              asset_pair, "_orderbook_", ob$timestamp, ".rds"))
  }

  if (filetype == "RData") {
    save(ob, file = paste0(exchange, "/",
                              asset_pair, "_orderbook_", ob$timestamp, ".RData"))
  }
}