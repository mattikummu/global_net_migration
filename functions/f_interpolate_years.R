f_interpolate_years <- function(dataIn, startCol) {
  
  #dataIn=birthsNational
  #startCol=4
  dataCols <- startCol:ncol(dataIn)
  cNames <- colnames(dataIn)[dataCols]
  
  # values to numeric
  dataIn <- dataIn %>%
    mutate(across(all_of(dataCols), ~ as.numeric(.x))) 
  
  # interpolate chunks one by one
  for (i in 1:(length(dataCols) - 1)) {
    
    ystart <- cNames[i]
    yend <- cNames[i+1]
    tsLength <- as.numeric(yend) - as.numeric(ystart)
    
    # compute linear change per year
    ipData <- dataIn %>%
      select(Country, Country.code, Type, dataCols[i], dataCols[i+1]) %>%
      mutate(deltaTotal = !!sym(yend) - !!sym(ystart),
             deltaYear = deltaTotal / tsLength)
    
    # interpolate
    for (y in 1:(tsLength - 1)) {
      
      ipCol <- as.character(as.numeric(ystart) + y)
      ipData <- ipData %>%
        mutate(!!ipCol := !!sym(ystart) + y * deltaYear)
      
    }
    
    if(i == (length(dataCols) - 1)) {
      # extrapolate two years 
      ipData <- ipData %>%
        mutate('2018' := !!sym(ystart) + 6 * deltaYear) %>% 
        mutate('2019' := !!sym(ystart) + 7 * deltaYear)
      
    } else{}
    
    # clean up and join back
    ipData <- ipData %>%
      select(-c(deltaTotal, deltaYear, !!sym(ystart), !!sym(yend)))
    dataIn <- dataIn %>%
      left_join(ipData, by = c("Country", "Country.code", "Type"))
    
  }
  
  # fix ordering; first meta, then years ordered
  dataIn <- dataIn %>%
    select(order(colnames(.))) %>%
    select(Country, Country.code, Type, everything())
  
  return (dataIn)
  
}