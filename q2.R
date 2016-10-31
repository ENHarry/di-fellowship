setwd('E:/PerDev/Data Incubator challenge/Citibike/')
for (i in 201501:201512)
     {
       url <- paste("https://s3.amazonaws.com/tripdata/", i, 
                    "-citibike-tripdata.zip", sep = "")
       filename <- paste("d", as.character(i), ".zip", sep="")
       download.file(url, filename)
       file_list = list.files(getwd())
       lapply(file_list, unzip)
       file.remove(filename)
       }
library(data.table)
library(dplyr)
library(readr)
library(lubridate)
multmerge = function(mypath){
  
  df <- list.files(full.names = TRUE) %>% 
    lapply(read_csv) %>% 
    bind_rows 
}
# calculating median duration in seconds
data <- as.data.table(multmerge(getwd()))
median_seconds = median(data$tripduration)
print(median_seconds)

# calculating number of rides starting and ending in the same location

samedist <- filter(data, `start station latitude` == `end station latitude` &
                    `start station longitude` == `end station longitude`)
samedist_frac = nrow(samedist)/nrow(data)
print(samedist_frac)

# calculating sd of distinct stations for all bikes
dtgroup <- data %>%
  group_by(bikeid)%>%
  summarise(n = n_distinct(`start station id`))
sd_stations <- summarise(dtgroup, sd(n))
mean_moves <- mean(dtgroup$n)
print(sd_stations)
print(mean_moves)

# calculating average distance in km
gcdist <- function(long1, lat1, long2, lat2) {
  R <- 6371 # Earth mean radius [km]
  dlon = long2 - long1
  dlat = lat2 - lat1
  a = (sin(dlat/2))^2 + cos(lat1) * cos(lat2) * (sin(dlon/2))^2
  c = 2 * atan2( sqrt(a), sqrt(1-a) )
  d = R * c
  return(d)
}
data$dist_km <- gcdist(data$`start station longitude`, data$`start station latitude`, 
                       data$`end station longitude`, data$`end station latitude`)
mean_km = mean(data$dist_km)
print(mean_km)

# calculating the range of the average duration of monthly trips
cdata <- data
cdata$starttime <- as.chron.IDate(cdata$starttime)
cdata$startmth <- month(cdata$starttime)

dagroup <- cdata %>%
  group_by(startmth) %>%
  summarise(m = mean(tripduration))
range_mean = max(dagroup$m)-min(dagroup$m)
print(range_mean)

# calculating fraction of rides exceed their corresponding time limit
sub_user <- data %>%
  group_by(usertype) %>%
  filter(tripduration > 2700 & usertype == "Subscriber")
cus_user <- data %>%
  group_by(usertype) %>%
  filter(tripduration > 1800 & usertype == "Customer")
rides_fraction = (nrow(cus_user)+nrow(sub_user))/nrow(data)
print(rides_fraction)

# calculating hourly usage for each station
usage <- data %>%
  group_by(`start station id`) %>%
  summarise(mean_secs = mean(tripduration), mean_hours = mean_secs/120)
system_hr = sum(usage$mean_hours)
largest_ratio = max(usage$mean_hours)/system_hr
print(largest_ratio)
