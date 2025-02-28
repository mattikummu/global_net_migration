f_fillmode <- function(v, na.rm) {  
  if (is.na(v[[5]])) {
    uniqv <- unique(v)
    uniqv <- uniqv[!is.na(uniqv)]
    fillval <- uniqv[which.max(tabulate(match(v, uniqv)))]
    return (fillval)
  } else {
    return (v[[5]])
  }
  
}