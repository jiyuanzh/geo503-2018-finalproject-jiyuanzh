# Importing data & cleaning & converting
library(tidyverse)
library(ggmap)
library(ggplot2)
library(dplyr)
library(spData)
library(sf)
library(rgdal)
library(rnoaa)
data(world)
albers <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
US.sf <- world %>% dplyr::filter(iso_a2 == "US")
US.sf.albers <- US.sf %>% st_transform(albers)
OTP <- read.csv("D:/UB/2018 Fall/503 R/geo503-2018-finalproject-jiyuanzh/data/OnTimeP.csv")
APdata <- read.csv("D:/UB/2018 Fall/503 R/geo503-2018-finalproject-jiyuanzh/data/AirportData.csv")
OTP.clean <- OTP %>% dplyr::filter(DEP_DELAY_NEW >= 1) %>% dplyr::filter(WEATHER_DELAY >= 1) %>% 
  dplyr::select(YEAR, MONTH, DAY_OF_MONTH, ORIGIN, ORIGIN_STATE_ABR, DEP_DELAY_NEW, WEATHER_DELAY)
OTP.sum <- OTP.clean %>% dplyr::select(ORIGIN, DEP_DELAY_NEW, WEATHER_DELAY) %>%
  group_by(ORIGIN) %>% summarize(total_delay = sum(DEP_DELAY_NEW), weather = sum(WEATHER_DELAY))
OTP.sum$ORIGIN <- sapply(OTP.sum$ORIGIN, as.character)
Airports <- APdata %>% select(AIRPORT, DISPLAY_AIRPORT_NAME, DISPLAY_AIRPORT_CITY_NAME_FULL, AIRPORT_COUNTRY_CODE_ISO, AIRPORT_STATE_CODE, LATITUDE, LONGITUDE)
A <- Airports %>% group_by(AIRPORT) %>% summarize(lat = mean(LATITUDE), lon = mean(LONGITUDE)) %>%
  dplyr::filter(lat != "NA") %>% dplyr::filter(lon != "NA")
A$AIRPORT <- sapply(A$AIRPORT, as.character)
Airport.sf <- st_as_sf(A, coords = c("lon", "lat"), crs = 4326)

# Obtain & Clean weather data
#st <- ghcnd_stations()
st <- read.csv("st.csv")
st.snow <- st %>% dplyr::filter(element %in% c("SNOW"))
write.csv(st.snow, file = "st_snow.csv")
st.snow.sf <- st_as_sf(st.snow, coords = c("longitude", "latitude"), crs = 4326)
st.snow.sf <- st.snow.sf %>% st_transform(albers)
st.snow.sf.us <- st_intersection(st.snow.sf, US.sf.albers) %>% dplyr::filter(last_year == 2018) %>% dplyr::filter(first_year > 2000)
st.snow.sf.us <- st.snow.sf.us %>% dplyr::filter(state != "NB" & state != "ON" & state != "BC") %>% dplyr::select(X, id, elevation, state, name, element, first_year, last_year)

#Inner join the On Time Performance data with Airport and make the first three plots
A_O.inner <- left_join(OTP.sum, A, by = c("ORIGIN" = "AIRPORT"))
A_O.inner.sf <- st_as_sf(A_O.inner, coords = c("lon", "lat"), crs = 4326)
p1 <- ggplot(US.sf) + geom_sf(aes(geometry = geom)) + geom_point(data = A_O.inner, aes(x = lon, y = lat))
p1
p2 <- ggplot(US.sf) + geom_sf(aes(geometry = geom)) + geom_point(data = A_O.inner, aes(x = lon, y = lat, size = weather))
p2
p3 <- ggplot(A_O.inner) + geom_point(aes(x = log(weather), y = log(total_delay))) + geom_smooth(method = loess, aes(x = log(weather), y = log(total_delay)))
p3

#Buffer the airport data and intersect with weather station data, station ID of choice
A_O.inner.buffer <- A_O.inner.sf %>% st_transform(albers) %>% st_buffer(5000)
st.snow.st.us.inx <- st_intersection(A_O.inner.buffer, st.snow.sf.us)
st.snow.st.us.inx.2 <- st.snow.st.us.inx %>% select(ORIGIN, id)
st_write(st.snow.st.us.inx.2, "data/st_snow_st_us_inx_2.shp")
st.snow.st.us.inx.2 <- st_read(system.file("data/st_snow_st_us_inx_2.shp", package = "sf"))

#REALLY importing weather data from NOAA
#weather2 <- meteo_pull_monitors(monitors = st.snow.st.us.inx$id, date_min = "2018-01-01", date_max = "2018-01-31", var = "SNOW")
weather2 <- read.csv("weather.csv")
weather2 <- weather2 %>% select(id, date, snow)
weather <- weather2 %>% spread(date, snow, fill = 0)
Airport.weather <- left_join(weather, st.snow.st.us.inx, by = c("id" = "id"))
Airport.weather2 <- left_join(weather2, st.snow.st.us.inx.2, by = c("id" = "id"))
OTP.weather <- OTP.clean %>% select(YEAR, MONTH, DAY_OF_MONTH, ORIGIN, WEATHER_DELAY) %>% dplyr::group_by_(.dots = c("DAY_OF_MONTH", "ORIGIN")) %>% summarise(W_DELAY = sum(WEATHER_DELAY))
OTP.weather.spread <- OTP.weather %>% spread(DAY_OF_MONTH, W_DELAY, fill = 0)
Airport.weather3 <- left_join(OTP.weather, Airport.weather2, by = c("ORIGIN" = "ORIGIN")) %>% dplyr::filter(snow != 0)

#Plot of weather delay vs snow
p4 <- ggplot(data = Airport.weather3) + geom_point(aes(x = snow, y = W_DELAY))
p4

#BUF as interested airports
BUF <- OTP.weather %>% dplyr::filter(ORIGIN == "BUF") %>% arrange(DAY_OF_MONTH)
p.BUF.1 <- ggplot(data = BUF) + geom_point(aes(x = DAY_OF_MONTH, y = W_DELAY))
p.BUF.1
BUF.weather <- weather2 %>% left_join(st.snow.st.us.inx.2, by = "id") %>% dplyr::filter(ORIGIN == "BUF") %>% na.omit() %>% separate(date, c("year", "month", "day"), sep = "-", convert = TRUE) %>% group_by(day) %>% summarise(SNOW = mean(snow))
p.BUF.2 <- ggplot(data = BUF.weather) + geom_point(aes(x = day, y = SNOW))
p.BUF.2
BUF.join <- left_join(BUF.weather, BUF, by = c("day" = "DAY_OF_MONTH")) %>% select(-ORIGIN)
BUF.join$W_DELAY <- BUF.join$W_DELAY %>% replace_na(0)
p.BUF <- ggplot(data = BUF.join) + geom_point(aes(x = SNOW, y = W_DELAY))
p.BUF

#EWR as interested airports
BOS <- OTP.weather %>% dplyr::filter(ORIGIN == "BOS") %>% arrange(DAY_OF_MONTH)
p.BOS.1 <- ggplot(data = BOS) + geom_point(aes(x = DAY_OF_MONTH, y = W_DELAY))
p.BOS.1
BOS.weather <- weather2 %>% left_join(st.snow.st.us.inx.2, by = "id") %>% dplyr::filter(ORIGIN == "BOS") %>% na.omit() %>% separate(date, c("year", "month", "day"), sep = "-", convert = TRUE) %>% group_by(day) %>% summarise(SNOW = mean(snow))
p.BOS.2 <- ggplot(data = BOS.weather) + geom_point(aes(x = day, y = SNOW))
p.BOS.2
BOS.join <- left_join(BOS.weather, BOS, by = c("day" = "DAY_OF_MONTH")) %>% select(-ORIGIN)
BOS.join$W_DELAY <- BOS.join$W_DELAY %>% replace_na(0)
p.BOS <- ggplot(data = BOS.join) + geom_point(aes(x = SNOW, y = W_DELAY))
p.BOS
