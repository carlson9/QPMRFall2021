#' @export
.checkTime <- function(mintime, maxtime){
  current <- as.numeric(format(Sys.time(), "%H"))
  if(current>=maxtime) Sys.sleep((24-current+mintime)*3600)
  if(current<=mintime) Sys.sleep((mintime-current)*3600)
}

