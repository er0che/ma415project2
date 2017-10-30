# cape may dataset

library(tidyverse)
library(stringr)
library(data.table)
library(lubridate)
library(dplyr)

str1 <- "http://www.ndbc.noaa.gov/view_text_file.php?filename=44009h"
str2 <- ".txt.gz&dir=data/historical/stdmet/"


years <- c(1984:2016)

urls <- str_c(str1, years, str2, sep = "")

filenames <- str_c("cm", years, sep = "")

N <- length(urls)

for (i in 1:N){
  assign(filenames[i], read_table(urls[i], col_names = TRUE))
  
  file <- get(filenames[i])
  
  colnames(file)[1] <-"YYYY"
  
  if(nchar(file[1,1]) == 2 & file[1,1] > 50 ) {
    file[1] <- lapply(file[1], function(x){str_c(19, x, sep = "")})
  }
  
  if(nchar(file[1,1]) == 2 & file[1,1] < 50 ) {
    file[1] <- lapply(file[1], function(x){str_c(20, x, sep = "")})
  }

  
  file <- file %>% select(YYYY, MM, DD, hh, ATMP, WTMP)
  
  if(i == 1){
    CM <- file
  }
  
  else{
    CM <- rbind.data.frame(CM, file)
  }
  
}

CM<-filter(CM, hh=="12", DD == '01' | DD =="15")

CM$date <- ymd(paste(CM$YYYY, CM$MM, CM$DD))

CM<-select(CM, date, ATMP, WTMP)

CM$ATMP <- apply(CM[,2], MARGIN = 2, function(x){ifelse(x == 999.0, NA, x)})
CM$ATMP <- apply(CM[,2], MARGIN = 2, function(x){ifelse(x == "999.0", NA, x)})
CM$ATMP <- apply(CM[,2], MARGIN = 2, function(x){ifelse(x == "99.0", NA, x)})
CM$WTMP <- apply(CM[,3], MARGIN = 2, function(x){ifelse(x == 999.0, NA, x)})
CM$WTMP <- apply(CM[,3], MARGIN = 2, function(x){ifelse(x == "999.0", NA, x)})

ggplot(data = CM, aes(x=date, y=ATMP)) + geom_point(data = CM, aes(x=date, y=ATMP)) + geom_smooth()

plot(CM)

