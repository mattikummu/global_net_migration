myFun_nationalData <-function(xlsIn){
  
  origData <- read.xlsx(xlsIn,"ESTIMATES", colNames = T, startRow = 17) %>%
    # unselect non-wanted columns
    select(-c(Index, Variant,Notes,Parent.code,paste0(seq(1950,1980,5),"-",seq(1955,1985,5)))) %>%
    
    as_tibble() %>%
    
    filter(Type == 'Country/Area')
  
}