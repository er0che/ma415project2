# WHAT TO DO
# proof read code according style guide rules
# add code comments
# CR - basic states & write up of that 
# MS - report on data cleaning
# experiment w/ visualization

# cape may dataset - Elise Roche

library(tidyverse)
library(stringr)
library(data.table)
library(lubridate)
library(dplyr)
library(xlsx)

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
  
  file$YYYY <- as.numeric(file$YYYY)
  
  file$MM <- as.numeric(file$MM)
  
  file$DD <- as.numeric(file$DD)
  
  file$hh <- as.numeric(file$hh)
  
  file$ATMP <- as.numeric(file$ATMP)
  
  file$WTMP <- as.numeric(file$WTMP)
  
  if(is.element("mm", colnames(file))) {
    file$mm <- as.numeric(file$mm)
    file <- file %>% filter(mm == 00 | mm == 50)
  }
  
  else {
    file$mm <- 00
  }
  
  file <- file %>% select(YYYY, MM, DD, hh, mm, ATMP, WTMP)
  
  file$date <- make_datetime(year = file$YYYY, month = file$MM, day = file$DD, hour = file$hh, min = file$mm)
  
  if(i == 1){
    CM <- file
  }
  
  else{
    CM <- rbind.data.frame(CM, file)
  }
  
}

CM <- filter(CM, hh == 12 & mm == 00 | hh == 11 & mm == 50)

CM$timediff <- make_datetime(hour = CM$hh, min = CM$mm) - make_datetime(hour = 12, min = 00) 
  
CM <- select(CM, date, timediff, ATMP, WTMP)

CM$ATMP <- apply(CM[,3], MARGIN = 2, function(x){ifelse(x == 999.0, NA, x)})
CM$ATMP <- apply(CM[,3], MARGIN = 2, function(x){ifelse(x == 99.0, NA, x)})
CM$WTMP <- apply(CM[,4], MARGIN = 2, function(x){ifelse(x == 999.0, NA, x)})
CM$WTMP <- apply(CM[,4], MARGIN = 2, function(x){ifelse(x == 99.0, NA, x)})

colnames(CM)[3] <- "Air Temp"
colnames(CM)[4] <- "Sea Temp"
colnames(CM)[1] <- "Date"
colnames(CM)[2] <- "Time from Noon"

CM$Group <- 1
CM$Type <- "Buoy"
CM$Lat <- 38.5
CM$Long <- 74.7

CM <- CM[c("Group", "Type", "Date", "Time from Noon", "Lat", "Long", "Sea Temp", "Air Temp")]


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
  
  file$YYYY <- as.numeric(file$YYYY)
  
  file$MM <- as.numeric(file$MM)
  
  file$DD <- as.numeric(file$DD)
  
  file$hh <- as.numeric(file$hh)
  
  file$ATMP <- as.numeric(file$ATMP)
  
  file$WTMP <- as.numeric(file$WTMP)
  
  if(is.element("mm", colnames(file))) {
    file$mm <- as.numeric(file$mm)
    file <- file %>% filter(mm == 00 | mm == 50)
  }
  
  else {
    file$mm <- 00
  }
  
  file <- file %>% select(YYYY, MM, DD, hh, mm, ATMP, WTMP)
  
  file$date <- make_datetime(year = file$YYYY, month = file$MM, day = file$DD, hour = file$hh, min = file$mm)
  
  if(i == 1){
    MR <- file
  }
  
  else{
    MR <- rbind.data.frame(MR, file)
  }
  
}

MR<-filter(MR, hh==12 & mm == 00 | hh == 11 & mm == 50)

MR$timediff <- make_datetime(hour = MR$hh, min = MR$mm) - make_datetime(hour = 12, min = 00) 

MR<-select(MR, date, timediff, ATMP, WTMP)

MR$ATMP <- apply(MR[,3], MARGIN = 2, function(x){ifelse(x == 999.0, NA, x)})
MR$ATMP <- apply(MR[,3], MARGIN = 2, function(x){ifelse(x == 99.0, NA, x)})
MR$WTMP <- apply(MR[,4], MARGIN = 2, function(x){ifelse(x == 999.0, NA, x)})
MR$WTMP <- apply(MR[,4], MARGIN = 2, function(x){ifelse(x == 99.0, NA, x)})

colnames(MR)[3] <- "Air Temp"
colnames(MR)[4] <- "Sea Temp"
colnames(MR)[1] <- "Date"
colnames(MR)[2] <- "Time from Noon"

MR$Group <- 1
MR$Type <- "Buoy"
MR$Lat <- 25.0
MR$Long <- 80.4

MR <- MR[c("Group", "Type", "Date", "Time from Noon", "Lat", "Long", "Sea Temp", "Air Temp")]

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
  
  file$YYYY <- as.numeric(file$YYYY)
  
  file$MM <- as.numeric(file$MM)
  
  file$DD <- as.numeric(file$DD)
  
  file$hh <- as.numeric(file$hh)
  
  file$ATMP <- as.numeric(file$ATMP)
  
  file$WTMP <- as.numeric(file$WTMP)
  
  if(is.element("mm", colnames(file))) {
    file$mm <- as.numeric(file$mm)
    file <- file %>% filter(mm == 00 | mm == 50)
  }
  
  else {
    file$mm <- 00
  }
  
  file <- file %>% select(YYYY, MM, DD, hh, mm, ATMP, WTMP)
  
  file$date <- make_datetime(year = file$YYYY, month = file$MM, day = file$DD, hour = file$hh, min = file$mm)
  
  if(i == 1){
    GB <- file
  }
  
  else{
    GB <- rbind.data.frame(GB, file)
  }
  
}

GB<-filter(GB, hh==12 & mm == 00 | hh == 11 & mm == 50)

GB$timediff <- make_datetime(hour = GB$hh, min = GB$mm) - make_datetime(hour = 12, min = 00) 

GB <- select(GB, date, timediff, ATMP, WTMP)

GB$ATMP <- apply(GB[,3], MARGIN = 2, function(x){ifelse(x == 999.0, NA, x)})
GB$ATMP <- apply(GB[,3], MARGIN = 2, function(x){ifelse(x == 99.0, NA, x)})
GB$WTMP <- apply(GB[,4], MARGIN = 2, function(x){ifelse(x == 999.0, NA, x)})
GB$WTMP <- apply(GB[,4], MARGIN = 2, function(x){ifelse(x == 99.0, NA, x)})

colnames(GB)[3] <- "Air Temp"
colnames(GB)[4] <- "Sea Temp"
colnames(GB)[1] <- "Date"
colnames(GB)[2] <- "Time from Noon"

GB$Group <- 1
GB$Type <- "Buoy"
GB$Lat <- 41.1
GB$Long <- 66.6

GB <- GB[c("Group", "Type", "Date", "Time from Noon", "Lat", "Long", "Sea Temp", "Air Temp")]

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
  
  file$YYYY <- as.numeric(file$YYYY)
  
  file$MM <- as.numeric(file$MM)
  
  file$DD <- as.numeric(file$DD)
  
  file$hh <- as.numeric(file$hh)
  
  file$ATMP <- as.numeric(file$ATMP)
  
  file$WTMP <- as.numeric(file$WTMP)
  
  if(is.element("mm", colnames(file))) {
    file$mm <- as.numeric(file$mm)
    file <- file %>% filter(mm == 00 | mm == 50)
  }
  
  else {
    file$mm <- 00
  }
  
  file <- file %>% select(YYYY, MM, DD, hh, mm, ATMP, WTMP)
  
  file$date <- make_datetime(year = file$YYYY, month = file$MM, day = file$DD, hour = file$hh, min = file$mm)
  
  if(i == 1){
    MG <- file
  }
  
  else{
    MG <- rbind.data.frame(MG, file)
  }
  
}

MG <- filter(MG, hh == 12 & mm == 00 | hh == 11 & mm == 50)

MG$timediff <- make_datetime(hour = MG$hh, min = MG$mm) - make_datetime(hour = 12, min = 00) 

MG <- select(MG, date, timediff, ATMP, WTMP)

MG$ATMP <- apply(MG[,3], MARGIN = 2, function(x){ifelse(x == 999.0, NA, x)})
MG$ATMP <- apply(MG[,3], MARGIN = 2, function(x){ifelse(x == 99.0, NA, x)})
MG$WTMP <- apply(MG[,4], MARGIN = 2, function(x){ifelse(x == 999.0, NA, x)})
MG$WTMP <- apply(MG[,4], MARGIN = 2, function(x){ifelse(x == 99.0, NA, x)})

colnames(MG)[3] <- "Air Temp"
colnames(MG)[4] <- "Sea Temp"
colnames(MG)[1] <- "Date"
colnames(MG)[2] <- "Time from Noon"

MG$Group <- 1
MG$Type <- "Buoy"
MG$Lat <- 25.9
MG$Long <- 89.7

MG <- MG[c("Group", "Type", "Date", "Time from Noon", "Lat", "Long", "Sea Temp", "Air Temp")]

write.xlsx(CM, "group1data.xlsx", sheetName = "Cape May - Buoy # 44009")
write.xlsx(MR, "group1data.xlsx", sheetName = "Mollases Reef - Buoy # MLRF1", append = TRUE)
write.xlsx(GB, "group1data.xlsx", sheetName = "Georges Bank - Buoy # 44011", append = TRUE)
write.xlsx(MG, "group1data.xlsx", sheetName = "Mid Gulf - Buoy # 42001", append = TRUE)
