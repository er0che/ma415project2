# WHAT TO DO
# col 1: team number, 1...
# col 2: type-reading, buoy.....
# col 3: time difference from noon, should be posix variables, A for ones w/o readings
# col 4: time
# col 5: location (lat)
# col 6: location (long)
# col 7: sea tep
# col 8: air temp


# cape may dataset - Elise Roche

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

CM<-filter(CM, hh=="12")

CM$date <- ymd(paste(CM$YYYY, CM$MM, CM$DD))

CM<-select(CM, date, ATMP, WTMP)

CM$region <- apply(CM[,4], MARGIN=2)
CM$ATMP <- apply(CM[,2], MARGIN = 2, function(x){ifelse(x == 999.0, NA, x)})
CM$ATMP <- apply(CM[,2], MARGIN = 2, function(x){ifelse(x == "999.0", NA, x)})
CM$ATMP <- apply(CM[,2], MARGIN = 2, function(x){ifelse(x == "99.0", NA, x)})
CM$WTMP <- apply(CM[,3], MARGIN = 2, function(x){ifelse(x == 999.0, NA, x)})
CM$WTMP <- apply(CM[,3], MARGIN = 2, function(x){ifelse(x == "999.0", NA, x)})

plot(CM)

# mollasses reef data - Brian Clare

str1 <- "http://www.ndbc.noaa.gov/view_text_file.php?filename=mlrf1h"
str2 <- ".txt.gz&dir=data/historical/stdmet/"

years <- c(1987:2016)

urls <- str_c(str1, years, str2, sep = "")

filenames <- str_c("mr", years, sep = "")

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
    MR <- file
  }
  
  else{
    MR <- rbind.data.frame(MR, file)
  }
  
}

MR<-filter(MR, hh=="12")

MR$date <- ymd(paste(MR$YYYY, MR$MM, MR$DD))

MR<-select(MR, date, ATMP, WTMP)

MR$ATMP <- apply(MR[,2], MARGIN = 2, function(x){ifelse(x == 999.0, NA, x)})
MR$ATMP <- apply(MR[,2], MARGIN = 2, function(x){ifelse(x == "999.0", NA, x)})
MR$ATMP <- apply(MR[,2], MARGIN = 2, function(x){ifelse(x == "99.0", NA, x)})
MR$WTMP <- apply(MR[,3], MARGIN = 2, function(x){ifelse(x == 999.0, NA, x)})
MR$WTMP <- apply(MR[,3], MARGIN = 2, function(x){ifelse(x == "999.0", NA, x)})
MR$WTMP <- apply(MR[,2], MARGIN = 2, function(x){ifelse(x == "99.0", NA, x)})

plot(MR)



# George's Bank - Melody Shaff


str1 <- "http://www.ndbc.noaa.gov/view_text_file.php?filename=44011h"
str2 <- ".txt.gz&dir=data/historical/stdmet/"

years <- c(1984:2013, 2015, 2016)

urls <- str_c(str1, years, str2, sep = "")

filenames <- str_c("gb", years, sep = "")

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
    GB <- file
  }
  
  else{
    GB <- rbind.data.frame(GB, file)
  }
  
}

GB<-filter(GB, hh=="12")

GB$date <- ymd(paste(GB$YYYY, GB$MM, GB$DD))

GB<-select(GB, date, ATMP, WTMP)

GB$ATMP <- apply(GB[,2], MARGIN = 2, function(x){ifelse(x == 999.0, NA, x)})
GB$ATMP <- apply(GB[,2], MARGIN = 2, function(x){ifelse(x == "999.0", NA, x)})
GB$ATMP <- apply(GB[,2], MARGIN = 2, function(x){ifelse(x == "99.0", NA, x)})
GB$WTMP <- apply(GB[,3], MARGIN = 2, function(x){ifelse(x == 999.0, NA, x)})
GB$WTMP <- apply(GB[,3], MARGIN = 2, function(x){ifelse(x == "999.0", NA, x)})
GB$WTMP <- apply(GB[,2], MARGIN = 2, function(x){ifelse(x == "99.0", NA, x)})

plot(GB)


# cape lookout - Carly Rose Willing

str1 <- "http://www.ndbc.noaa.gov/view_text_file.php?filename=44011h"
str2 <- ".txt.gz&dir=data/historical/stdmet/"

years <- c(1984:2016)

urls <- str_c(str1, years, str2, sep = "")

filenames <- str_c("cl", years, sep = "")

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
    CL <- file
  }
  
  else{
    CL <- rbind.data.frame(CL, file)
  }
  
}

CL<-filter(CL, hh=="12")

CL$date <- ymd(paste(CL$YYYY, CL$MM, CL$DD))

CL<-select(CL, date, ATMP, WTMP)

CL$ATMP <- apply(CL[,2], MARGIN = 2, function(x){ifelse(x == 999.0, NA, x)})
CL$ATMP <- apply(CL[,2], MARGIN = 2, function(x){ifelse(x == "999.0", NA, x)})
CL$ATMP <- apply(CL[,2], MARGIN = 2, function(x){ifelse(x == "99.0", NA, x)})
CL$WTMP <- apply(CL[,3], MARGIN = 2, function(x){ifelse(x == 999.0, NA, x)})
CL$WTMP <- apply(CL[,3], MARGIN = 2, function(x){ifelse(x == "999.0", NA, x)})
CL$WTMP <- apply(CL[,2], MARGIN = 2, function(x){ifelse(x == "99.0", NA, x)})

plot(CL)
