# Load libraries
library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)
library(mapdata)
library(maps)

# Set working directory
getwd()
setwd("/Users/zmcro/OneDrive/Documents/Cyclistic Portfolio Project 2/Orig")

# =========================
# LOAD DATASETS
# =========================
Aug_2020 <- read_csv("202008-divvy-tripdata.csv")
Sep_2020 <- read_csv("202009-divvy-tripdata.csv") 
Oct_2020 <- read_csv("202010-divvy-tripdata.csv")
Nov_2020 <- read_csv("202011-divvy-tripdata.csv")
Dec_2020 <- read_csv("202012-divvy-tripdata.csv")
Jan_2021 <- read_csv("202101-divvy-tripdata.csv")
Feb_2021 <- read_csv("202102-divvy-tripdata.csv")
Mar_2021 <- read_csv("202103-divvy-tripdata.csv")
Apr_2021 <- read_csv("202104-divvy-tripdata.csv")
May_2021 <- read_csv("202105-divvy-tripdata.csv")
Jun_2021 <- read_csv("202106-divvy-tripdata.csv")
Jul_2021 <- read_csv("202107-divvy-tripdata.csv")

# =========================
# UNDERSTAND DATA AND COMBINE INTO SINGLE DATAFRAME
# =========================
colnames(Aug_2020)
colnames(Sep_2020)
colnames(Oct_2020)
colnames(Nov_2020)
colnames(Dec_2020)
colnames(Jan_2021)
colnames(Feb_2021)
colnames(Mar_2021)
colnames(Apr_2021)
colnames(May_2021)
colnames(Jun_2021)
colnames(Jul_2021)

# Checking to see if there are any inconsistencies with
# the structure of the data
str(Aug_2020)
str(Sep_2020)
str(Oct_2020)
str(Nov_2020)
str(Dec_2020)
str(Jan_2021)
str(Feb_2021)
str(Mar_2021)
str(Apr_2021)
str(May_2021)
str(Jun_2021)
str(Jul_2021)

Aug_2020$start_station_id <- as.character(Aug_2020$start_station_id)
Aug_2020$end_station_id <- as.character(Aug_2020$end_station_id)
Sep_2020$start_station_id <- as.character(Sep_2020$start_station_id)
Sep_2020$end_station_id <- as.character(Sep_2020$end_station_id)
Oct_2020$start_station_id <- as.character(Oct_2020$start_station_id)
Oct_2020$end_station_id <- as.character(Oct_2020$end_station_id)
Nov_2020$start_station_id <- as.character(Nov_2020$start_station_id)
Nov_2020$end_station_id <- as.character(Nov_2020$end_station_id)

# Combine dataframes into one dataframe
all_trips <- bind_rows(Aug_2020, Sep_2020, Oct_2020, Nov_2020, Dec_2020, 
                       Jan_2021, Feb_2021, Mar_2021, Apr_2021, May_2021,
                       Jun_2021, Jul_2021)


# =========================
# CLEAN AND ADD DATA TO PREPARE FOR ANALYSIS
# =========================
# Inspecting the new table
colnames(all_trips) # Listing the column names
nrow(all_trips) # Displaying number of rows in dataframe
dim(all_trips) # Printing the dimensions of the data
head(all_trips) # See first six rows of the dataframe
str(all_trips) # See list of columns and their data types
summary(all_trips) # Statistical summary of data (numerics primarily)

# Add columns to list day, month, and year of each ride
all_trips$date <- as.Date(all_trips$started_at)
all_trips$month <- format(all_trips$date, "%m")
all_trips$day <- format(all_trips$date, "%d")
all_trips$year <- format(all_trips$date, "%Y")
all_trips$weekday <- format(all_trips$date, "%A")
all_trips$hhmmss <- format(all_trips$started_at, format="%H:%M:%s")
all_trips$hhmm <- format(all_trips$started_at, format="%H:%M")
all_trips$hh <- format(all_trips$started_at, format="%H")
all_trips$endhh <- format(all_trips$ended_at, format="%H")


# Compute the ride_length using start/end times
all_trips$ride_length <- difftime(all_trips$ended_at, all_trips$started_at)
is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)

# Compute the distance using haversine formula with the lat/long for start/end
# https://rstudio-pubs-static.s3.amazonaws.com/379437_445892091c66412b8e67d92aeb768946.html
my_dist <- function(long1, lat1, long2, lat2) {
  rad <- pi/180
  a1 <- lat1*rad
  a2 <- long1*rad
  b1 <- lat2*rad
  b2 <- long2*rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1)*cos(b1)*(sin(dlon/2))^2
  c <- 2*atan2(sqrt(a), sqrt(1 - a))
  R <- 6378137
  d <- R*c
  m <- round(d/1600, digits = 2)
  return(m)
}

# Creating column for haversine computation
all_trips$distHav <- my_dist(all_trips$start_lng, all_trips$start_lat,
                             all_trips$end_lng, all_trips$end_lat)

# Filter out data that ruins analysis and sort by yyyy-mm-dd and hh:mm:ss
all_trips_sorted <- filter(all_trips, !(start_station_name=="WATSON TESTING - DIVVY" | ride_length<0 | distHav<0)) %>% 
  arrange(year, month, day, hhmmss)



# =========================
# DESCRIPTIVE ANALYSIS
# =========================
# Questions to ask:
# What is the difference in number of rides per month between members and casuals?
# What is the difference in number of rides per month by day between members and casuals?
# Is there a difference in ride_length between members and casuals?
# Is there a difference in distance traveled between members and casuals?
# Is there a popular start/end station between members and casuals?
# Are there differences in ride_length in regards to weekdays?
# Are there differences in number of rides in regards to weekdays?
# Is there a difference in the number of rides across each month between members and casuals?
# Is there a difference in the types of bikes used between members and casuals?
# Is there a difference in the time of day for rides between members and casuals?


###############################################################
# QUESTION: Examining difference in number of rides per month
###############################################################
# number of rides per month
all_trips_sorted %>% 
  mutate(month = lubridate::month(started_at, label=TRUE)) %>% 
  group_by(member_casual, month) %>% 
  summarise(number_of_rides = n(), avg_duration=mean(ride_length)) %>% 
  arrange(member_casual)

# number of rides per month (graph)
all_trips_sorted %>% 
  mutate(month = lubridate::month(started_at, label=TRUE)) %>% 
  group_by(member_casual, month) %>% 
  summarise(number_of_rides = n(), avg_duration=mean(ride_length)) %>% 
  ggplot(aes(x=month, y=number_of_rides, fill=member_casual)) + 
  geom_col(position="dodge")+
  labs(title="Number of Rides per Month") + xlab("Month") + ylab("Number of Rides") +
  guides(fill=guide_legend(title="Rider Type"))

######################################################################
# QUESTION: Looking at difference in number of rides per month by day
######################################################################
# number of rides per month by day (graph)
num_rides_dow <-all_trips_sorted %>% 
  mutate(month = lubridate::month(started_at, label=TRUE)) %>% 
  group_by(member_casual, month, day) %>% 
  summarise(number_of_rides = n(), avg_duration=mean(ride_length)) %>% 
  ggplot(aes(x=day, y=number_of_rides, fill=member_casual, shape = member_casual)) + 
  geom_col(position="fill") + 
  facet_wrap(~month) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
num_rides_dow +
  labs(title="Number of Rides (%) per Month by Day") + xlab("Day") + ylab("Number of Rides") +
  guides(fill=guide_legend(title="Rider Type"))

###################################################################################
# QUESTION: Looking at ride length between members and casuals
###################################################################################
mean(all_trips_sorted$ride_length)
median(all_trips_sorted$ride_length)
max(all_trips_sorted$ride_length)
min(all_trips_sorted$ride_length)
summary(all_trips_sorted$ride_length)

# Looking at ride_length between members and casuals
aggregate(all_trips_sorted$ride_length ~ all_trips_sorted$member_casual, FUN=mean)
aggregate(all_trips_sorted$ride_length ~ all_trips_sorted$member_casual, FUN=median)
aggregate(all_trips_sorted$ride_length ~ all_trips_sorted$member_casual, FUN=max)
aggregate(all_trips_sorted$ride_length ~ all_trips_sorted$member_casual, FUN=min)

#####################################################################
# QUESTION: Looking at distance traveled between members and casuals
#####################################################################
mean(all_trips_sorted$distHav)
median(all_trips_sorted$distHav)
max(all_trips_sorted$distHav)
min(all_trips_sorted$distHav)
summary(all_trips_sorted$distHav)

# Looking at distance traveled between members and casuals
aggregate(all_trips_sorted$distHav ~ all_trips_sorted$member_casual, FUN=mean)
aggregate(all_trips_sorted$distHav ~ all_trips_sorted$member_casual, FUN=median)
aggregate(all_trips_sorted$distHav ~ all_trips_sorted$member_casual, FUN=max)
aggregate(all_trips_sorted$distHav ~ all_trips_sorted$member_casual, FUN=min)

########################################################################################
# QUESTION: Seeing if there is a popular start/end station between members and casuals
########################################################################################
# Looking into what is the most popular start stations between members and casuals
popular_start <- all_trips_sorted %>% 
  group_by(member_casual,start_station_name) %>% 
  count() %>% 
  arrange(desc(n))
popular_start

# Looking into what is the most popular end stations between members and casuals
popular_end <- all_trips_sorted %>% 
  filter(!is.na(end_station_name)) %>% 
  group_by(member_casual, end_station_name) %>% 
  count() %>% 
  arrange(desc(n))
popular_end

# Looking into the non-specified end stations
all_trips_sorted %>% 
  filter(is.na(end_station_name)) %>% 
  group_by(member_casual, end_station_name) %>% 
  count() %>% 
  arrange(desc(n))

# Looking at map of Cook County start stations
usa <- map_data("usa")
states <- map_data("state")
chicago <- subset(states, region=="illinois")

chicago_plot <- ggplot(chicago, aes(long,lat, group=group)) + 
  coord_fixed(1)+ geom_polygon(color='black', fill='gray') 
counties <- map_data("county")

illi_counties <- subset(counties, region=='illinois')

# Start stations used by casuals
start_stat_casuals <- all_trips_sorted %>% 
  filter(member_casual=="casual"&ride_length>60) %>% 
  select(c(ride_id,rideable_type,start_station_name,start_lng, start_lat,member_casual,
           date,month,day,year,weekday,ride_length,distHav))

starts_casuals <- chicago_plot + geom_polygon(data=illi_counties, fill=NA,color="white") + 
  geom_polygon(fill=NA,color="black") + coord_cartesian(xlim = c(xlim=-88.3,-87.5),ylim=c(41.4,42.2)) + 
  geom_point(data=start_stat_casuals, aes(start_lng,start_lat),pch=0, size=0.5, color = 'blue', inherit.aes = FALSE)  

starts_casuals +
  labs(title="Start Station Locations for Casual Users") + xlab("Longitude") + ylab("Latitude")

# start stations used by members
start_stat_members <- all_trips_sorted %>% 
  filter(member_casual=="member"&ride_length>60) %>% 
  select(c(ride_id,rideable_type,start_station_name,start_lng, start_lat,member_casual,
           date,month,day,year,weekday,ride_length,distHav))

starts_members <- chicago_plot + geom_polygon(data=illi_counties, fill=NA,color="white") + 
  geom_polygon(fill=NA,color="black") + coord_cartesian(xlim = c(xlim=-88.3,-87.5),ylim=c(41.4,42.2)) + 
  geom_point(data=start_stat_members, aes(start_lng,start_lat),pch=0, size=0.5, color = 'red', inherit.aes = FALSE)  

starts_members + 
  labs(title="Start Station Locations for Members") + xlab("Longitude") + ylab("Latitude")


# bikes left at non-specified end stations for casuals
casual_no_station <- all_trips_sorted %>% 
  filter(member_casual=="casual"&ride_length>60&is.na(end_station_name)) %>% 
  group_by(end_lng, end_lat) %>% 
  select(c(ride_id,rideable_type,end_station_name,end_lng, end_lat,member_casual,
           date,month,day,year,weekday,ride_length,distHav))

# bikes left at non-specified end stations for members
member_no_station <- all_trips_sorted %>% 
  filter(member_casual=="member"&ride_length>60&is.na(end_station_name)) %>% 
  group_by(end_lng, end_lat) %>% 
  select(c(ride_id,rideable_type,end_station_name,end_lng, end_lat,member_casual,
           date,month,day,year,weekday,ride_length,distHav))

# bikes left at non-specified end stations for all riders
no_station <- chicago_plot + geom_polygon(data=illi_counties, fill=NA,color="white") + 
  geom_polygon(fill=NA,color="black") + coord_cartesian(xlim = c(xlim=-88.3,-87.5),ylim=c(41.4,42.2)) + 
  geom_point(data=casual_no_station, aes(end_lng,end_lat),pch=0,size=0.5,color="blue", inherit.aes = FALSE) +
  geom_point(data=member_no_station, aes(end_lng,end_lat),pch=0,size=0.5,color="red", inherit.aes = FALSE) +
  layer(geom = "point", stat="identity", position="identity")
no_station +
  labs(title="Non-Specified End Locations") + xlab("Longitude") + ylab("Latitude") +
  guides(fill=guide_legend(title="Rider Type"))

no_station_facet <- chicago_plot + geom_polygon(data=illi_counties, fill=NA,color="white") + 
  geom_polygon(fill=NA,color="black") + coord_cartesian(xlim = c(xlim=-88.3,-87.5),ylim=c(41.4,42.2)) + 
  geom_point(data=casual_no_station, aes(end_lng,end_lat),pch=0,size=0.5, color='blue', inherit.aes = FALSE) +
  geom_point(data=member_no_station, aes(end_lng,end_lat),pch=0,size=0.5, color='red', inherit.aes = FALSE) +
  facet_wrap(~member_casual)
no_station_facet +
  labs(title="Non-Specified End Locations") + xlab("Longitude") + ylab("Latitude") +
  guides(fill=guide_legend(title="Rider Type"))

###############################################################################
# QUESTION: Looking at the ride_length per members and casuals on a given day
###############################################################################

aggregate(all_trips_sorted$ride_length ~ all_trips_sorted$member_casual+all_trips_sorted$weekday, FUN=mean)
all_trips_sorted$weekday <- ordered(all_trips_sorted$weekday, levels=c("Sunday", "Monday", "Tuesday", 
                                                                       "Wednesday", "Thursday", "Friday", 
                                                                       "Saturday"))
aggregate(all_trips_sorted$ride_length ~ all_trips_sorted$member_casual+all_trips_sorted$weekday, FUN=mean)

# avg duration vs dow
avg_dur_dow <- all_trips_sorted %>% 
  mutate(dow = wday(started_at, label=TRUE)) %>% 
  group_by(member_casual, dow) %>% 
  summarise(number_of_rides = n(), avg_duration=mean(ride_length)) %>% 
  arrange(member_casual, dow) %>% 
  ggplot(aes(x=dow, y=avg_duration, fill=member_casual)) + geom_col(position="dodge")

avg_dur_dow +
  labs(title="Average Duration of Ride by Day of Week") + xlab("Day of Week") + ylab("Average Duration (Seconds)") +
  guides(fill=guide_legend(title="Rider Type"))

##################################################################################
# QUESTION: Looking at the number of rides per members and casuals on a given day
##################################################################################
# dow: number of rides with avg duration
all_trips_sorted %>% 
  mutate(dow = wday(started_at, label=TRUE)) %>% 
  group_by(member_casual, dow) %>% 
  summarise(number_of_rides = n(), avg_duration=mean(ride_length)) %>% 
  arrange(member_casual, dow)

# number of rides vs dow
num_rides_dow <- all_trips_sorted %>% 
  mutate(dow = wday(started_at, label=TRUE)) %>% 
  group_by(member_casual, dow) %>% 
  summarise(number_of_rides = n(), avg_duration=mean(ride_length)) %>% 
  arrange(member_casual, dow) %>% 
  ggplot(aes(x=dow, y=number_of_rides, fill=member_casual)) + geom_col(position="dodge")

num_rides_dow +
  labs(title="Number of Rides by Day of Week") + xlab("Day of Week") + ylab("Number of Rides") +
  guides(fill=guide_legend(title="Rider Type"))

#################################################################################
# QUESTION: Looking at the ride_length per members and casuals in a given month
#################################################################################

aggregate(all_trips_sorted$ride_length ~ all_trips_sorted$member_casual+all_trips_sorted$month, FUN=mean)

# avg duration per month
avg_dur_month <- all_trips_sorted %>% 
  mutate(month = lubridate::month(started_at, label=TRUE)) %>% 
  group_by(member_casual, month) %>% 
  summarise(number_of_rides = n(), avg_duration=mean(ride_length)) %>% 
  arrange(member_casual, month) %>% 
  ggplot(aes(x=month, y=avg_duration, fill=member_casual)) + geom_col(position="dodge")

avg_dur_month + 
  labs(title="Average Duration of Ride per Month") + xlab("Month") + ylab("Average Duration (Seconds)") +
  guides(fill=guide_legend(title="Rider Type"))

#################################################################################################
# QUESTION: Looking at if there is a difference in the type of bike used by members and casuals 
#################################################################################################
popular_bike_type <- all_trips_sorted %>% 
  group_by(member_casual,rideable_type) %>% 
  count() %>%
  arrange(member_casual, desc(n))
popular_bike_type

popular_bike_month <- all_trips_sorted %>% 
  group_by(rideable_type, member_casual,month) %>% 
  count() %>% 
  arrange(member_casual, desc(n))
popular_bike_month

# by type of bike
bike_type_riders <- all_trips_sorted %>% 
  group_by(rideable_type, member_casual) %>% 
  summarise(num_bike = n()) %>% 
  arrange(member_casual) %>% 
  ggplot(aes(x=member_casual, y=num_bike, fill=member_casual,show.legend = FALSE)) + 
  geom_col(position="dodge") +
  facet_wrap(~rideable_type)

bike_type_riders + 
  labs(title="Bike Type Usage by Rider Type") + xlab("Rider Type") + ylab("Bike Usage") +
  theme(legend.position = "none")


# by type of bike per month
bike_type_month <- all_trips_sorted %>% 
  group_by(rideable_type, member_casual,month) %>% 
  summarise(num_bike = n()) %>% 
  arrange(member_casual) %>% 
  ggplot(aes(x=month, y=num_bike, fill=member_casual)) + geom_col(position="dodge") +
  facet_grid(~rideable_type)

bike_type_month + 
  labs(title="Bike Type Usage by Month") + xlab("Month") + ylab("Bike Usage") +
  guides(fill=guide_legend(title="Rider Type"))

########################################################################################################
# QUESTION: Looking at if there is a difference in the time of day for rides between members and casuals
########################################################################################################
tod_casual <- all_trips_sorted %>% 
  filter(member_casual=="casual") %>% 
  group_by(member_casual, month, hh) %>% 
  summarise(num_rides=n()) %>% 
  ggplot(aes(x=hh, y=num_rides,group=1)) + geom_path(color="red") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  facet_wrap(~month)
  
tod_casual + 
  labs(title="Time of Day Usage by Casual Users") + xlab("Hour") + ylab("Number of Rides")

tod_members <- all_trips_sorted %>% 
  filter(member_casual=="member") %>% 
  group_by(member_casual, month, hh) %>% 
  summarise(num_rides=n()) %>% 
  ggplot(aes(x=hh, y=num_rides,group=1)) + geom_path(color="red") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  facet_wrap(~month)
  
tod_members + 
  labs(title="Time of Day Usage by Members") + xlab("Hour") + ylab("Number of Rides")


# Looking into when rides usually end between casuals and  members
tod_casual_end <- all_trips_sorted %>% 
  filter(member_casual=="casual") %>% 
  group_by(member_casual, month, endhh) %>% 
  summarise(num_rides=n()) %>% 
  ggplot(aes(x=endhh, y=num_rides,group=1)) + geom_path(color="red") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  facet_wrap(~month)

tod_casual_end + 
  labs(title="Time of Day Usage by Casual Users") + xlab("Hour") + ylab("Number of Rides")

tod_members_end <- all_trips_sorted %>% 
  filter(member_casual=="member") %>% 
  group_by(member_casual, month, endhh) %>% 
  summarise(num_rides=n()) %>% 
  ggplot(aes(x=endhh, y=num_rides,group=1)) + geom_path(color="red") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  facet_wrap(~month)

tod_members_end + 
  labs(title="Time of Day Usage by Members") + xlab("Hour") + ylab("Number of Rides")
