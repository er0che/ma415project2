# WHAT TO DO
# proof read code according style guide rules
# add code comments
# CR - basic states & write up of that 
# MS - report on data cleaning
# experiment w/ visualization

# Setting up some libraries we'll need

options(java.parameters = "- Xmx1024m")


library(tidyverse)
library(stringr)
library(lubridate)
library(xlsx)

# For Cape May
# These sections of code will repeat for each location, so I'm only commenting once

# First we set up the urls of the buoy data tables from the NDBC website

str1 <- "http://www.ndbc.noaa.gov/view_text_file.php?filename=44009h"
str2 <- ".txt.gz&dir=data/historical/stdmet/"

years <- c(1984:2016)

urls <- str_c(str1, years, str2, sep = "")

# Now that we havve urls, let's say what the filenames for each year will be

filenames <- str_c("cm", years, sep = "")

# And how many we have

N <- length(urls)

for (i in 1:N){
  
  # For each year of our data, get the table from that url and call it "file"
  
  assign(filenames[i], read_table(urls[i], col_names = TRUE))
  
  file <- get(filenames[i])
  
  # Before 1998 the year column is only 2 digits, YY, so let's get those to all match
  
  colnames(file)[1] <-"YYYY"
  
  # If the first year entry is 2 digits, put 19 in front of it
  
  if(nchar(file[1,1]) == 2 & file[1,1] > 50 ) {
    file[1] <- lapply(file[1], function(x){str_c(19, x, sep = "")})
  }
  
  # Since we turned it into a string by doing that, let's get it back to a number
  
  file$YYYY <- as.numeric(file$YYYY)
  
  # And then make sure everything else is numbers
  
  file$MM <- as.numeric(file$MM)
  
  file$DD <- as.numeric(file$DD)
  
  file$hh <- as.numeric(file$hh)
  
  file$ATMP <- as.numeric(file$ATMP)
  
  file$WTMP <- as.numeric(file$WTMP)
  
  # Early years didn't have a minutes column
  # For those with minutes, we want to add it in with an entry at zero
  # Otherwise, we only want 0 minutes if possible
  # But some years have entries at 50 minutes instead
  
  if(is.element("mm", colnames(file))) {
    file$mm <- as.numeric(file$mm)
    file <- file %>% filter(mm == 00 | mm == 50)
  }
  
  else {
    file$mm <- 00
  }
  
  # Pulling out just the columns we care about, then making the date column
  
  file <- file %>% select(YYYY, MM, DD, hh, mm, ATMP, WTMP)
  
  file$date <- make_datetime(year = file$YYYY, month = file$MM, day = file$DD, 
                             hour = file$hh, min = file$mm)
  
  # Putting our temporary 'file' into the main dataframe for this location
  
  if(i == 1){
    CM <- file
  }
  
  else{
    CM <- rbind.data.frame(CM, file)
  }
  
}

# Now we have a few other actions to perform
# We filter out so we have just one entry for each day, the closest to noon

CM <- filter(CM, hh == 12 & mm == 00 | hh == 11 & mm == 50)

# Then make the time difference column

CM$timediff <- make_datetime(hour = CM$hh, min = CM$mm) - make_datetime(hour = 12, 
                                                                        min = 00) 
# Then we'll filter down to the columns we really want, recode the NAs, 
# and rename the columns  

CM <- select(CM, date, timediff, ATMP, WTMP)

CM$ATMP <- apply(CM[,3], MARGIN = 2, function(x){ifelse(x == 999.0, NA, x)})
CM$ATMP <- apply(CM[,3], MARGIN = 2, function(x){ifelse(x == 99.0, NA, x)})
CM$WTMP <- apply(CM[,4], MARGIN = 2, function(x){ifelse(x == 999.0, NA, x)})
CM$WTMP <- apply(CM[,4], MARGIN = 2, function(x){ifelse(x == 99.0, NA, x)})

colnames(CM)[3] <- "air_temp"
colnames(CM)[4] <- "sea_temp"
colnames(CM)[1] <- "date_time"
colnames(CM)[2] <- "time_diff"

# There are a few more columns to put in

CM$team_num <- 1
CM$reading_type <- "buoy"
CM$Lat <- 38.5
CM$Lon <- 74.7

# And then reordering the columns

CM <- CM[c("team_num", "reading_type", "date_time", "time_diff", "Lat", "Lon", 
           "sea_temp", "air_temp")]

# Then we do it all again for the next location
# Molasses Reef

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
  
  file$date <- make_datetime(year = file$YYYY, month = file$MM, day = file$DD, 
                             hour = file$hh, min = file$mm)
  
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

colnames(MR)[3] <- "air_temp"
colnames(MR)[4] <- "sea_temp"
colnames(MR)[1] <- "date_time"
colnames(MR)[2] <- "time_diff"

MR$team_num <- 1
MR$reading_type <- "buoy"
MR$Lat <- 25.0
MR$Lon <- 80.4

MR <- MR[c("team_num", "reading_type", "date_time", "time_diff", "Lat", "Lon", 
           "sea_temp", "air_temp")]

# Georges Bank

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
  
  file$date <- make_datetime(year = file$YYYY, month = file$MM, day = file$DD, 
                             hour = file$hh, min = file$mm)
  
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

colnames(GB)[3] <- "air_temp"
colnames(GB)[4] <- "sea_temp"
colnames(GB)[1] <- "date_time"
colnames(GB)[2] <- "time_diff"

GB$team_num <- 1
GB$reading_type <- "buoy"
GB$Lat <- 41.1
GB$Lon <- 66.6

GB <- GB[c("team_num", "reading_type", "date_time", "time_diff", "Lat", "Lon", 
            "sea_temp", "air_temp")]

# Mid-Gulf

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
  
  file$date <- make_datetime(year = file$YYYY, month = file$MM, day = file$DD, 
                             hour = file$hh, min = file$mm)
  
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

colnames(MG)[3] <- "air_temp"
colnames(MG)[4] <- "sea_temp"
colnames(MG)[1] <- "date_time"
colnames(MG)[2] <- "time_diff"

MG$team_num <- 1
MG$reading_type <- "buoy"
MG$Lat <- 25.9
MG$Lon <- 89.7

MG <- MG[c("team_num", "reading_type", "date_time", "time_diff", "Lat", "Lon", 
           "sea_temp", "air_temp")]

# Finally, we need to put these dataframes into an xlsx file

write.xlsx2(CM, "group1data.xlsx", sheetName = "Cape May - Buoy # 44009")
write.xlsx2(MR, "group1data.xlsx", sheetName = "Molasses Reef - Buoy # MLRF1", append = TRUE)
write.xlsx2(GB, "group1data.xlsx", sheetName = "Georges Bank - Buoy # 44011", append = TRUE)
write.xlsx2(MG, "group1data.xlsx", sheetName = "Mid Gulf - Buoy # 42001", append = TRUE)

# CM Air and Sea

ggplot(data = CM) + geom_point(mapping = aes(x = sea_temp, y = air_temp), color="lightblue") + 
  labs(title="Cape May Air Temperature vs. Sea Temperature", x="Sea Temperature (°C)", y="Air Temperature (°C)")

# MR Air and Sea

ggplot(data = MR) + geom_point(mapping = aes(x = sea_temp, y = air_temp), color="darkolivegreen1") + 
  labs(title="Molasses Reef Air Temperature vs. Sea Temperature", x="Sea Temperature (°C)", y="Air Temperature (°C)")

# MG Air and Sea

ggplot(data = MG) + geom_point(mapping = aes(x = sea_temp, y = air_temp), color="lightpink1") + 
  labs(title="Mid Gulf Air Temperature vs. Sea Temperature", x="Sea Temperature (°C)", y="Air Temperature (°C)")

# GB Air and Sea

ggplot(data = GB) + geom_point(mapping = aes(x = sea_temp, y = air_temp), color="khaki") + 
  labs(title="Georges Bank Air Temperature vs. Sea Temperature", x="Sea Temperature (°C)", y="Air Temperature (°C)")
