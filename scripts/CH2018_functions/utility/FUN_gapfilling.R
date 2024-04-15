# *** title:gapfilling function
# *** description: fills NA, if 1 NA in row with mean of previous and next value,
# ***              if more than 1 NA in a row it takes the values from the year before,
# ***              if the year before is also NA (will just happen if the first year contains NA)it takes the mean of the next 7 years of the same date (NAs in this values will be ignored)
# *** details: used for temperature data, needs to get a time vector and a data vector
# ***
# *** Flavian Tschurr,09.10.2019

#### Arguments
## data: vector, numeric, your data series
## time: vector, dates (form YYYY-mm-dd) (best format as.POSIXlt())

gapfilling <- function(data,time){
  
  require(lubridate)
  time <- as.POSIXlt(time)
  vectNAs <- which(is.na(data))
  counter <- 2
  for(vectNA in vectNAs){
    
    if(vectNA+1 == vectNAs[counter] & counter <= length(vectNAs)){
      
      missingDate <- time[vectNA]%m+% years(-1)
      oneYrBefore <- subset(time,time<missingDate)
      place <- as.numeric(length(oneYrBefore))
      if(place > 365){
        data[vectNA] <- data[place]
      }else {
        gapFiller <- NULL
        for (j in c(1:7)) {
          
          
          oneYrAfter <-subset(time,time < time[vectNA]%m+% years(j))
          if(!is.na(data[length(oneYrAfter)])){
            data[vectNA] <- data[length(oneYrAfter)]
            break
          }
          
        }
        
        
      }
      
      
    }else{
      # simpel case: 1 NA
      ## mean der stelle davor und nach dem NA wird berechnet
      toMean<- c(data[vectNA-1],data[vectNA+1], na.rm=TRUE)
      data[vectNA] <- mean(toMean)
    }
    # print(paste("gapfilling position: ",counter, sep = ""))
    counter <- counter +1
    
  }
  if(is.na(data[length(data)])){
    data[length(data)] <- data[(length(data)-1)]
  }
  
  return(data)
  
}
