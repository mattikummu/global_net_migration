myFun_nationalDataWB <-function(xlsIn){
  #xlsIn <- "../data_in/API_SP.DYN.CBRT.IN_DS2_en_excel_v2_2595037.xlsx"
  origData <- read.xlsx(xlsIn,"Data", colNames = T, startRow = 4) %>%
    # unselect non-wanted columns
    select(-c(Indicator.Name, Indicator.Name,Indicator.Code,paste0(c(1960:1989,2020)))) %>%
    
    as_tibble() 
  
  #names(origData) = c("Country","Country.code","Type",'1992','1997','2002','2007','2012','2017')
}