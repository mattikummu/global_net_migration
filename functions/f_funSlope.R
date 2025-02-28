f_funSlope <- function(x) {
  
  time <- 1:length(timeSteps)
  if (is.na(x[1])) {
    return (NA)
  } else {
    x.a = as.array(x)
    m = lm(x.a ~ time)
    return (summary(m)$coefficients[2])
  }
}