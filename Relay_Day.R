#import packages
library(ggplot2)
library(plyr)
library(dplyr, quietly = T, warn.conflicts = F)
#Import relay data by day (including subscriber/customer count)
relay_day <- read.csv(file="C:/Users/jess/OneDrive/Grad School/Conferences/RDay2018/RelayBike/SystemData/RELAY_DAY.csv", header=TRUE, sep=",")

#Format variables of interest
relay_day$season  <- factor(relay_day$season, labels = c("Spring", "Summer", "Fall", "Winter"))

# Converting integer to factor - Day
relay_day$workday <- factor(relay_day$workday, labels=c("Not a workday","Workday"))
relay_day$Start_Date <-as.POSIXct(relay_day$Start_Date, format="%m/%d/%Y")
relay_day$month <- factor(relay_day$month, labels = c("October","November","December","
                                                        January","February","March",
                                                        "April","May","June","July"))
#relay_day$conditions <- factor(relay_day$conditions, labels = c("Good","Poor","Bad"))

#Drop missing
colSums(is.na(relay_day))
relay_day <- relay_day[complete.cases(relay_day),]

#Log of counts - day
relay_day$customer_log <- log(relay_day$no_trips_customer) 
relay_day$subscriber_log <- log(relay_day$no_trips_subscriber) 

#Season summary
#Customers
# Get the average count of bikes rent by season, day - customers
season_summary <- ddply(relay_day,.(season,Start_Date),
                        summarise, count = mean(no_trips_customer))
ggplot(relay_day, aes(x = factor(Start_Date), y = count, colour = season)) +
  geom_point(data = season_summary, aes(group = season)) +
  geom_line(data = season_summary, aes(group = season)) +
  #scale_x_datetime(breaks = date_breaks("4 hours"), labels=date_format("%I:%M %p")) +
  scale_x_discrete("Day") +
  scale_y_continuous("Count") +
  theme_minimal() +
  ggtitle("Bike Rentals By Season- Customer") + 
  theme(plot.title=element_text(size=18))

#Subscribers
# Get the average count of bikes rent by season, hour
season_summary <- ddply(relay_counts,.(season,hour),
                        summarise, count = mean(no_trips_subscriber))
ggplot(relay_counts, aes(x = hour, y = count, colour = season)) +
  geom_point(data = season_summary, aes(group = season)) +
  geom_line(data = season_summary, aes(group = season)) +
  scale_x_discrete("Hour") +
  scale_y_continuous("Count") +
  theme_minimal() +
  ggtitle("Bike Rentals By Season- Subscriber") + 
  theme(plot.title=element_text(size=18))

# Get the average count of bikes rent by day, hour
day_summary <- ddply(relay_hour,.(weekday),
                     summarise, count = mean(count))
ggplot(relay_day, aes(x = weekday, y = count, colour = weekday)) +
  geom_point(data = day_summary, aes(group=weekday)) +
  geom_line(data = day_summary, aes(group=weekday)) +
  scale_x_discrete("Hour") +
  scale_y_continuous("Count") +
  theme_minimal() +
  ggtitle("Bike Rentals by Weekday")

#Customers
day_summary <- ddply(relay_counts,.(weekday,hour),
                     summarise, count = mean(no_trips_customer))
ggplot(relay_counts, aes(x = hour, y = count, colour = weekday)) +
  geom_point(data = day_summary, aes(group=weekday)) +
  geom_line(data = day_summary, aes(group=weekday)) +
  scale_x_discrete("Hour") +
  scale_y_continuous("Count") +
  theme_minimal() +
  ggtitle("Bike Rentals by Weekday - Customers")

#Subscribers
day_summary <- ddply(relay_counts,.(weekday,hour),
                     summarise, count = mean(no_trips_subscriber))
ggplot(relay_counts, aes(x = hour, y = count, colour = weekday)) +
  geom_point(data = day_summary, aes(group=weekday)) +
  geom_line(data = day_summary, aes(group=weekday)) +
  scale_x_discrete("Hour") +
  scale_y_continuous("Count") +
  theme_minimal() +
  ggtitle("Bike Rentals by Weekday - Subscribers")

weather_summary <- ddply(relay_counts,.(conditions,hour),
                         summarise, count = mean(count))
ggplot(relay_counts, aes(x = hour, y = count, colour = conditions)) +
  geom_point(data = weather_summary, aes(group = conditions)) +
  geom_line(data = weather_summary, aes(group = conditions)) +
  scale_x_discrete("Hour") +
  scale_y_continuous("Count") +
  theme_minimal() +
  ggtitle("Bike Rentals by Weather Conditions") + 
  theme(plot.title=element_text(size=18))

#Customers
weather_summary <- ddply(relay_counts,.(conditions,hour),
                         summarise, count = mean(no_trips_customer))
ggplot(relay_counts, aes(x = hour, y = count, colour = conditions)) +
  geom_point(data = weather_summary, aes(group = conditions)) +
  geom_line(data = weather_summary, aes(group = conditions)) +
  scale_x_discrete("Hour") +
  scale_y_continuous("Count") +
  theme_minimal() +
  ggtitle("Bike Rentals by Weather Conditions - Customers") + 
  theme(plot.title=element_text(size=18))

#Subscribers
weather_summary <- ddply(relay_counts,.(conditions,hour),
                         summarise, count = mean(no_trips_subscriber))
ggplot(relay_counts, aes(x = hour, y = count, colour = conditions)) +
  geom_point(data = weather_summary, aes(group = conditions)) +
  geom_line(data = weather_summary, aes(group = conditions)) +
  scale_x_discrete("Hour") +
  scale_y_continuous("Count") +
  theme_minimal() +
  ggtitle("Bike Rentals by Weather Conditions - Subscribers") + 
  theme(plot.title=element_text(size=18))

#Time and Temperature
# Tweak these to show something else on the axes
x_axis <- "Start_Date"
y_axis <- "no_trips_customer"
color  <- "temp" # for example swap this to "humidity"
p <- ggplot(relay_day, aes_string(x=x_axis, y=y_axis, color=color)) +
  geom_point(position=position_jitter(w=0.0, h=0.4)) +
  theme_light(base_size=20) +
  xlab("Day") +
  #scale_x_datetime(breaks = date_breaks("4 hours"), labels=date_format("%I:%M %p")) + 
  ylab("Number of Bike Rentals") +
  scale_colour_gradientn("Temp (°F)", colours=c("#5e4fa2", "#3288bd", "#66c2a5", "#abdda4", "#e6f598", "#fee08b", "#fdae61", "#f46d43", "#d53e4f", "#9e0142")) +
  ggtitle("Bike Rentals by Day and Temperature") +
  theme(plot.title=element_text(size=18))
p
ggsave("C:/Users/jess/OneDrive/Grad School/Conferences/RDay2018/RelayBike/SystemData/Images/day_and_temperature.tiff", p)

#Windspeed
color  <- "windspeed" # for example swap this to "humidity"
p <- ggplot(relay_day, aes_string(x=x_axis, y=y_axis, color=color)) +
  geom_point(position=position_jitter(w=0.0, h=0.4)) +
  theme_light(base_size=20) +
  xlab("Date") +
  #scale_x_datetime(breaks = date_breaks("4 hours"), labels=date_format("%I:%M %p")) + 
  ylab("Number of Bike Rentals") +
  scale_colour_gradientn("Windspeed (MPH)", colours=c("#5e4fa2", "#3288bd", "#66c2a5", "#abdda4", "#e6f598", "#fee08b", "#fdae61", "#f46d43", "#d53e4f", "#9e0142")) +
  ggtitle("Bike Rentals by Date and Windspeed") +
  theme(plot.title=element_text(size=18))
p
ggsave("C:/Users/jess/OneDrive/Grad School/Conferences/RDay2018/RelayBike/SystemData/Images/date_and_wind.tiff", p)

#Humidity
color  <- "humidity" # for example swap this to "humidity"
p <- ggplot(relay_counts[relay_counts$workday=="Workday",], aes_string(x=x_axis, y=y_axis, color=color)) +
  geom_point(position=position_jitter(w=0.0, h=0.4)) +
  theme_light(base_size=20) +
  xlab("Hour of the Day") +
  #scale_x_datetime(breaks = date_breaks("4 hours"), labels=date_format("%I:%M %p")) + 
  ylab("Number of Bike Rentals") +
  scale_colour_gradientn("Humidity(relative %)", colours=c("#5e4fa2", "#3288bd", "#66c2a5", "#abdda4", "#e6f598", "#fee08b", "#fdae61", "#f46d43", "#d53e4f", "#9e0142")) +
  ggtitle("Bike Rentals by Hour and Windspeed") +
  theme(plot.title=element_text(size=18))
p
ggsave("C:/Users/jess/OneDrive/Grad School/Conferences/RDay2018/RelayBike/SystemData/Images/time_and_humid.tiff", p)

#Visibility
color  <- "visibility" # for example swap this to "humidity"
p <- ggplot(relay_counts[relay_counts$workday=="Workday",], aes_string(x=x_axis, y=y_axis, color=color)) +
  geom_point(position=position_jitter(w=0.0, h=0.4)) +
  theme_light(base_size=20) +
  xlab("Hour of the Day") +
  #scale_x_datetime(breaks = date_breaks("4 hours"), labels=date_format("%I:%M %p")) + 
  ylab("Number of Bike Rentals") +
  scale_colour_gradientn("Visibility(miles)", colours=c("#5e4fa2", "#3288bd", "#66c2a5", "#abdda4", "#e6f598", "#fee08b", "#fdae61", "#f46d43", "#d53e4f", "#9e0142")) +
  ggtitle("Bike Rentals by Hour and Windspeed") +
  theme(plot.title=element_text(size=18))
p
ggsave("C:/Users/jess/OneDrive/Grad School/Conferences/RDay2018/RelayBike/SystemData/Images/time_and_visibility.tiff", p)


