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

  if(is.element("mm", colnames(file))) {
    file <- file %>% filter((hh = 12 & mm = 0) | (hh = 11 & mm = 50))
  }
  
  
  file <- file %>% select(YYYY, MM, DD, hh, mm, ATMP, WTMP)
  
  if(i == 1){
    CM <- file
  }
  
  else{
    CM <- rbind.data.frame(CM, file)
  }
  
}

CM<-filter(CM, hh=="12")

CM$date <- ymd(paste(CM$YYYY, CM$MM, CM$DD, CM$hh, CM$mm))

CM<-select(CM, date, ATMP, WTMP)

CM$ATMP <- apply(CM[,2], MARGIN = 2, function(x){ifelse(x == 999.0, NA, x)})
CM$ATMP <- apply(CM[,2], MARGIN = 2, function(x){ifelse(x == "999.0", NA, x)})
CM$ATMP <- apply(CM[,2], MARGIN = 2, function(x){ifelse(x == "99.0", NA, x)})
CM$WTMP <- apply(CM[,3], MARGIN = 2, function(x){ifelse(x == 999.0, NA, x)})
CM$WTMP <- apply(CM[,3], MARGIN = 2, function(x){ifelse(x == "999.0", NA, x)})

colnames(CM)[2] <- "Air Temp"
colnames(CM)[3] <- "Sea Temp"
colnames(CM)[1] <- "Time"

CM$Group <- 1
CM$Type <- "Buoy"
CM$Lat <- 38.5
CM$Long <- 74.7

CM <- CM[c("Group", "Type", "Time", "Lat", "Long", "Sea Temp", "Air Temp")]


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
  
  if(is.element("mm", colnames(file))) {
    file <- file %>% filter((hh = 12 & mm = 0) | (hh = 11 & mm = 50))
  }
  
  ### add in statement adding "0" or NA column to datasets w.o. minutes
  
  
  file <- file %>% select(YYYY, MM, DD, hh, mm, ATMP, WTMP)
  
  if(i == 1){
    MR <- file
  }
  
  else{
    MR <- rbind.data.frame(MR, file)
  }
  
}

MR<-filter(MR, hh=="12")

MR$date <- ymd(paste(MR$YYYY, MR$MM, MR$DD, MR$hh, MR$mm))

MR<-select(MR, date, ATMP, WTMP)

MR$ATMP <- apply(MR[,2], MARGIN = 2, function(x){ifelse(x == 999.0, NA, x)})
MR$ATMP <- apply(MR[,2], MARGIN = 2, function(x){ifelse(x == "999.0", NA, x)})
MR$ATMP <- apply(MR[,2], MARGIN = 2, function(x){ifelse(x == "99.0", NA, x)})
MR$WTMP <- apply(MR[,3], MARGIN = 2, function(x){ifelse(x == 999.0, NA, x)})
MR$WTMP <- apply(MR[,3], MARGIN = 2, function(x){ifelse(x == "999.0", NA, x)})
MR$WTMP <- apply(MR[,3], MARGIN = 2, function(x){ifelse(x == "99.0", NA, x)})

colnames(MR)[2] <- "Air Temp"
colnames(MR)[3] <- "Sea Temp"
colnames(MR)[1] <- "Time"

MR$Group <- 1
MR$Type <- "Buoy"
MR$Lat <- 25.0
MR$Long <- 80.4

MR <- MR[c("Group", "Type", "Time", "Lat", "Long", "Sea Temp", "Air Temp")]

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
  
  if(is.element("mm", colnames(file))) {
    file <- file %>% filter((hh = 12 & mm = 0) | (hh = 11 & mm = 50))
  }
  
  file <- file %>% select(YYYY, MM, DD, hh, mm, ATMP, WTMP)
  
  if(i == 1){
    GB <- file
  }
  
  else{
    GB <- rbind.data.frame(GB, file)
  }
  
}

GB<-filter(GB, hh=="12")

GB$date <- ymd(paste(GB$YYYY, GB$MM, GB$DD, GB$hh, GB$mm))

GB<-select(GB, date, ATMP, WTMP)

GB$ATMP <- apply(GB[,2], MARGIN = 2, function(x){ifelse(x == 999.0, NA, x)})
GB$ATMP <- apply(GB[,2], MARGIN = 2, function(x){ifelse(x == "999.0", NA, x)})
GB$ATMP <- apply(GB[,2], MARGIN = 2, function(x){ifelse(x == "99.0", NA, x)})
GB$WTMP <- apply(GB[,3], MARGIN = 2, function(x){ifelse(x == 999.0, NA, x)})
GB$WTMP <- apply(GB[,3], MARGIN = 2, function(x){ifelse(x == "999.0", NA, x)})
GB$WTMP <- apply(GB[,3], MARGIN = 2, function(x){ifelse(x == "99.0", NA, x)})

colnames(GB)[2] <- "Air Temp"
colnames(GB)[3] <- "Sea Temp"
colnames(GB)[1] <- "Time"

GB$Group <- 1
GB$Type <- "Buoy"
GB$Lat <- 41.1
GB$Long <- 66.6

GB <- GB[c("Group", "Type", "Time", "Lat", "Long", "Sea Temp", "Air Temp")]

plot(GB)

# cape lookout - Carly Rose Willing

str1 <- "http://www.ndbc.noaa.gov/view_text_file.php?filename=42001h"
str2 <- ".txt.gz&dir=data/historical/stdmet/"

years <- c(1984:2016)

urls <- str_c(str1, years, str2, sep = "")

filenames <- str_c("mg", years, sep = "")

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
  
  if(is.element("mm", colnames(file))) {
    file <- file %>% filter(hh = 12 & mm = 0) | (hh = 11 & mm = 50))
  }
  
  file <- file %>% select(YYYY, MM, DD, hh, mm, ATMP, WTMP)
  
  if(i == 1){
    MG <- file
  }
  
  else{
    MG <- rbind.data.frame(MG, file)
  }
  
}

MG<-filter(MG, hh=="12")

MG$date <- ymd(paste(MG$YYYY, MG$MM, MG$DD, MG$hh, MG$mm))

MG<-select(MG, date, ATMP, WTMP)

MG$ATMP <- apply(MG[,2], MARGIN = 2, function(x){ifelse(x == 999.0, NA, x)})
MG$ATMP <- apply(MG[,2], MARGIN = 2, function(x){ifelse(x == "999.0", NA, x)})
MG$ATMP <- apply(MG[,2], MARGIN = 2, function(x){ifelse(x == "99.0", NA, x)})
MG$WTMP <- apply(MG[,3], MARGIN = 2, function(x){ifelse(x == 999.0, NA, x)})
MG$WTMP <- apply(MG[,3], MARGIN = 2, function(x){ifelse(x == "999.0", NA, x)})
MG$WTMP <- apply(MG[,3], MARGIN = 2, function(x){ifelse(x == "99.0", NA, x)})

colnames(MG)[2] <- "Air Temp"
colnames(MG)[3] <- "Sea Temp"
colnames(MG)[1] <- "Time"

MG$Group <- 1
MG$Type <- "Buoy"
MG$Lat <- 25.9
MG$Long <- 89.7

MG <- MG[c("Group", "Type", "Time", "Lat", "Long", "Sea Temp", "Air Temp")]

plot(MG)