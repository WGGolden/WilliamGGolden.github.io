# Packages ####
library(MASS)
library(forecast)
library(tidyverse)
library(prophet)
library(reshape2)
library(leaflet)
library(gganimate)

# Cleaning Data for exploration ####

df1 <- read.csv("./Data/Raw/2000-2009.csv")

df1$YEAR <- df1$DATE %>% str_trunc(4,ellipsis = "") %>% as.numeric()

Clean1 <- df1 %>% 
  group_by(YEAR,NAME, LATITUDE,LONGITUDE) %>% 
  summarize(TotalPrecip=sum(PRCP,na.rm = TRUE),
            AvgPrecip=mean(PRCP,na.rm=TRUE))

df2 <- read.csv("./Data/Raw/2010-Present.csv")

df2$YEAR <- df2$DATE %>% str_trunc(4,ellipsis = "") %>% as.numeric()

Clean2 <- df2 %>% 
  group_by(YEAR,NAME, LATITUDE,LONGITUDE) %>% 
  summarize(TotalPrecip=sum(PRCP,na.rm = TRUE),
            AvgPrecip=mean(PRCP,na.rm=TRUE))

df3 <- read.csv("./Data/Raw/1990-1999.csv")

df3$YEAR <- df3$DATE %>% str_trunc(4,ellipsis = "") %>% as.numeric()

Clean3 <- df3 %>% 
  group_by(YEAR,NAME, LATITUDE,LONGITUDE) %>% 
  summarize(TotalPrecip=sum(PRCP,na.rm = TRUE),
            AvgPrecip=mean(PRCP,na.rm=TRUE))

df4 <- read.csv("./Data/Raw/1980-1989.csv")

df4$YEAR <- df4$DATE %>% str_trunc(4,ellipsis = "") %>% as.numeric()

Clean4 <- df4 %>% 
  group_by(YEAR,NAME, LATITUDE,LONGITUDE) %>% 
  summarize(TotalPrecip=sum(PRCP,na.rm = TRUE),
            AvgPrecip=mean(PRCP,na.rm=TRUE))



df5 <- read.csv("./Data/Raw/1970-1979.csv")

df5$YEAR <- df5$DATE %>% str_trunc(4,ellipsis = "") %>% as.numeric()

Clean5 <- df5 %>% 
  group_by(YEAR,NAME, LATITUDE,LONGITUDE) %>% 
  summarize(TotalPrecip=sum(PRCP,na.rm = TRUE),
            AvgPrecip=mean(PRCP,na.rm=TRUE))



Clean <- rbind(Clean1,Clean2,Clean3,Clean4,Clean5)

rm(list=setdiff(ls(), "Clean"))

# Graphing Exploratory Code ####

q <- Clean %>% 
  group_by(YEAR) %>% 
  summarize(TotalPrcp = mean(TotalPrecip)) %>% 
  ggplot(aes(x=YEAR,y=TotalPrcp))+
  geom_line() +
  labs(x="Year",
       y="Total Precipiation (Inches)",
       title = "Total Precipiation over Time in Utah County")
q + 
  geom_point() +
  transition_reveal(YEAR)+
  theme_classic()

p <- Clean %>% 
  group_by(YEAR, NAME) %>% 
  summarize(TotalPrcp = mean(TotalPrecip)) %>% 
  ggplot(aes(x=NAME, y=TotalPrcp))+
  geom_col()+
  labs(x="Station Name",
       y="Total Precipiation (Inches)")+
  theme(axis.text.x = (element_text(angle = 60, hjust = 1)))


p+transition_time(YEAR) +
  labs(title = "Year: {frame_time}")


# Weather Station Mapping ####

library(sf)
library(leaflet)
devtools::install_github("ropensci/USAboundaries")
devtools::install_github("ropensci/USAboundariesData")
library(USAboundaries)

m <- leaflet() %>% 
  setView(lng = -111.6952, lat = 40.1542,zoom = 8) %>% 
  addTiles()
m  

# extract polygon info for Utah County border
ut_counties <- us_counties(states = "Utah",resolution = "high") 

#clean extra column
ut_counties <- ut_counties[,-9]
ut_counties %>% 
  filter(name == "Utah")


utah_co <- ut_counties %>% 
  filter(name == "Utah")

ut_co_poly <- utah_co$geometry
latlon <- ut_co_poly %>% unlist()
long <- latlon[1:(length(latlon)/2)]
lat <- latlon[(length(long)+1):length(latlon)]

# add station circles, sized by total precipiaton

stations <- Clean %>% filter(YEAR==1970)


stations_long <- stations$LONGITUDE
stations_lat <- stations$LATITUDE


m %>% 
  addPolygons(lng = long,
              lat = lat,
              color = "Grey",
              fillOpacity = .1) %>% 
  addCircles(lng = stations_long,
             lat=stations_lat,
             radius = stations$AvgPrecip*40000,
             label = paste0(stations$NAME,": Total Precipiation (inches) in 1970= ",
                            stations$TotalPrecip),
             fillColor = "Blue",
             color = "Purple")

stations2 <- Clean %>% filter(YEAR==2021)


stations_long <- stations2$LONGITUDE
stations_lat <- stations2$LATITUDE


m %>% 
  addPolygons(lng = long,
              lat = lat,
              color = "Grey",
              fillOpacity = .1) %>% 
  addCircles(lng = stations_long,
             lat=stations_lat,
             radius = stations$AvgPrecip*40000,
             label = paste0(stations$NAME,": Total Precipiation (inches) in 2021= ",
                            stations$TotalPrecip),
             fillColor = "Blue",
             color = "Purple")

# Data load and cleaning for prophet ####

# Loading packages

df1 <- read.csv("./Data/Raw/2000-2009.csv")

Clean1 <- df1 %>% 
  group_by(DATE,NAME) %>% 
  summarize(TotalPrecip=sum(PRCP,na.rm = TRUE),
            AvgPrecip=mean(PRCP,na.rm=TRUE))

df2 <- read.csv("./Data/Raw/2010-Present.csv")

Clean2 <- df2 %>% 
  group_by(DATE,NAME) %>% 
  summarize(TotalPrecip=sum(PRCP,na.rm = TRUE),
            AvgPrecip=mean(PRCP,na.rm=TRUE))

df3 <- read.csv("./Data/Raw/1990-1999.csv")

Clean3 <- df3 %>% 
  group_by(DATE,NAME) %>% 
  summarize(TotalPrecip=sum(PRCP,na.rm = TRUE),
            AvgPrecip=mean(PRCP,na.rm=TRUE))

df4 <- read.csv("./Data/Raw/1980-1989.csv")

Clean4 <- df4 %>% 
  group_by(DATE,NAME) %>% 
  summarize(TotalPrecip=sum(PRCP,na.rm = TRUE),
            AvgPrecip=mean(PRCP,na.rm=TRUE))



df5 <- read.csv("./Data/Raw/1970-1979.csv")

Clean5 <- df5 %>% 
  group_by(DATE,NAME) %>% 
  summarize(TotalPrecip=sum(PRCP,na.rm = TRUE),
            AvgPrecip=mean(PRCP,na.rm=TRUE))



Clean <- rbind(Clean1,Clean2,Clean3,Clean4,Clean5)

rm(list=setdiff(ls(), "Clean"))






df1 <- Clean %>% 
  select(TotalPrecip,DATE)

df1 <- rename(df1, ds=DATE)

df1 <- rename(df1, y=TotalPrecip)


##

m <- prophet(df1)

future <- make_future_dataframe(m, periods = 365)

forecast <- predict(m, future)

plot(m, forecast)

prophet_plot_components(m, forecast)
