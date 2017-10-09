

cast_char2date <- function(y, m, d) {
  as.Date(sprintf("%s-%s-%s", y, m, d), format = "%Y-%m-%d")
}

cast_char2dd <- function(deg, min, sec, dir) {
  # multicolumn character to decimal degree
  stopifnot(all(sapply(list(deg, min, sec), is.numeric)))
  stopifnot(is.character(dir))
  stopifnot(length(table(sapply(list(deg, min, sec, dir), length)))==1)
  
  out <- rep(as.numeric(NA), length(deg))
  cc  <- complete.cases(deg, min, sec, dir)
  
  char <- sprintf("%sd %s' %s\" %s", deg[cc], min[cc], sec[cc], dir[cc])
  dms <- sp::char2dms(char)
  out[cc] <- as.numeric(dms)
  out
}

cast(..., from = "", to = "") {
  
}