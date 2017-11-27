

install.packages("lubridate")

library(lubridate)
library(tidyverse)
library(ggplot2)
library(stringr)
library(gsubfn)
library(magrittr)
library(gdata)


uber_req_data<- read.csv("Uber Request Data.csv", stringsAsFactors = FALSE, na.strings = c("NA",""))

data<- read.csv("loan.csv", stringsAsFactors = FALSE, na.strings = c("NA",""))


## renaming columns

colnames(uber_req_data)<-c("req_id","pickup_point","driver_id","status","request_timestamp","drop_timestamp")
str(uber_req_data)


##---- checking for any dublicate rows---

length(unique(uber_req_data$req_id))

length(unique(uber_req_data$req_id)) == nrow(uber_req_data) # if true i.e. no duplicate value


## putting "00" in place of 'seconds' which is missing in reqsuest timestamp and drop time stamp

Log_metric <- str_detect(uber_req_data$request_timestamp, pattern = "[0-9][0-9]:[0-9][0-9]:[0-9][0-9]")

uber_req_data$request_timestamp[which(Log_metric == F)] <- paste(uber_req_data$request_timestamp[which(Log_metric == F)], "00", sep = ":")

uber_req_data$request_timestamp<- gsub("NA:00","NA",uber_req_data$request_timestamp)



Log_metric_2 <- str_detect(uber_req_data$drop_timestamp, pattern = "[0-9][0-9]:[0-9][0-9]:[0-9][0-9]")

uber_req_data$drop_timestamp[which(Log_metric == F)] <- paste(uber_req_data$drop_timestamp[which(Log_metric == F)], "00", sep = ":")

uber_req_data$drop_timestamp<- gsub("NA:00","NA",uber_req_data$drop_timestamp)

typeof(uber_req_data$drop_timestamp)
typeof(uber_req_data$driver_id)

### converting request and drop timestamp strings to datetime object.



# Default format: "%Y-%m-%d %H:%M:%S"

uber_req_data$request_timestamp <- as.POSIXct(uber_req_data$request_timestamp, format="%y.%m.%d %H:%M:%S")

typeof(uber_req_data$request_timestamp)

#using POSIXct() ?as.POSIXct()

uber_req_data$drop_timestamp <- as.POSIXct(uber_req_data$drop_timestamp,format="%y.%m.%d %H:%M:%S" )
typeof(uber_req_data$drop_timestamp)


# creating new metrics from date time object

uber_req_data$request_hour <- format(uber_req_data$request_timestamp, "%H")

uber_req_data$request_day  <- weekdays(as.Date(uber_req_data$request_timestamp))


uber_req_data$year <- format(uber_req_data$request_timestamp, "%Y")

typeof(uber_req_data$request_hour)

hrs<- as.numeric(uber_req_data$request_hour)
Hrs<- data.frame(hrs) ## just for to get a glance of data.

typeof(hrs)

##uber_req_data$date<- as.Date(uber_req_data$request_timestamp)



uber_req_data<- uber_req_data %>% mutate(TimeSlots_Demand_Supply= ifelse(status== "Trip Completed","Supply","Demand"))

typeof(uber_req_data$demand_supply_satus)

uber_req_Demand<- uber_req_data %>% filter(TimeSlots_Demand_Supply == "Demand")


#### to visualise the frequency of requests that get cancelled or show 'no cars available'

uber_req_data<- uber_req_data %>% mutate(shifts = ifelse(hrs %in% 0:4, "mid night",
                                  ifelse(hrs %in% 4:8, "early morning",
                                         ifelse(hrs %in% 8:12, "morning",
                                                ifelse(hrs %in% 12:16, "after noon",
                                                       ifelse(hrs %in% 16:20, "evening", "night"))))))



ggplot(uber_req_Demand,aes(status)) + geom_bar()

##### identify the most problematic types of requests (city to airport / airport to city etc.) and the time slots (early mornings, late evenings etc.) using plots

ggplot(uber_req_data,aes(x=int_rate_bin_percent$int_rate_bin_charged_off ,fill= status)) + geom_bar(position='stack') + facet_wrap( ~ pickup_point) +labs(x="TimeSlots")


#####Find the time slots when the highest gap exists.Find the types of requests (city-airport or airport-city) for which the gap is the most severe in the identified time slots
ggplot(uber_req_data,aes(x=shifts,fill=TimeSlots_Demand_Supply))+ geom_bar(position = 'dodge')+facet_wrap( ~ pickup_point)+ labs(x="TimeSlots", y="Number of Requests")





### univariated analysis & segmenteg analyisis-

status_analysis<- uber_req_data %>% group_by(status) %>% summarise( count=n())

##analysis with dplyr::










