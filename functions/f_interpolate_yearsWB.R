f_interpolate_yearsWB <- function(dataIn, startCol) {
  
  #dataIn=birthsNational
  #startCol=3
  dataCols <- startCol:ncol(dataIn)
  cNames <- colnames(dataIn)[dataCols]
  
  # values to numeric
  dataIn <- dataIn %>%
    mutate(across(all_of(dataCols), ~ as.numeric(.x))) 
  
  # interpolate chunks one by one
  for (i in 1:nrow(dataIn)) {
    ipData <- as.numeric(dataIn[i,dataCols] )
    
    if(is.na(sum(ipData,na.rm = T))) {
      # do nothing
      ipData_interp <-  ipData
    }else{
      ipData_interp <- zoo::na.approx(ipData,na.rm=F)
    }
    
    dataIn[i,dataCols] <- t(as_tibble(as.numeric(ipData_interp)))
  }
  
  return (dataIn)
}
