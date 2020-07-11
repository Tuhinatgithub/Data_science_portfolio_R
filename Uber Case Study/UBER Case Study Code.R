#--------------------------------------Start-----------------------------------------------

#Load the uber request data file into R.Don't convert the strings into factors
uber_data <- read.csv("Uber Request Data.csv",stringsAsFactors = FALSE,na.strings = TRUE)

#load the required packages 
require(tidyr)
require(dplyr)
require(ggplot2)
require(scales)

# Case conversion
uber_data$Pickup.point <- tolower(uber_data$Pickup.point)
uber_data$Status <- tolower(uber_data$Status)

#Extract the first two characters of Request time column 
#Save it as a seperate column of the dataframe
uber_data <- separate(uber_data, Request.timestamp, into = c("request_date","Request_hour"), sep = " ")
uber_data <- separate(uber_data, Request_hour, into = c("Request_hour"), sep = ":")

#convert the separated charcters to data type numeric
uber_data$Request_hour <- as.numeric(uber_data$Request_hour)

# Plot the number of cabs requested in a particular hour for all 05 days
# Pickup points will be displayed in two colors
Hourwise_request_count <- ggplot(uber_data,aes(x=factor(Request_hour),fill=factor(Pickup.point)))

#Add title and lables to the plot and save it as a object
Hourly_demand_plot <- Hourwise_request_count+geom_bar(stat='count',position = "dodge")+
  ggtitle("Hourly Demand for Uber Cabs")+
  labs(x="Time in Hours", y="Number of Cabs Requested")+
  labs(fill="Pickup Point")

#view the plot
Hourly_demand_plot

# Generate a sequence of numbers from 0 to 23 for hour interval
# save it as a vector
Request_hour <- c(0:23)

# Create a vector of names of time slots
Time_Slot <- c("Pre_Morning","Morning_Rush","Day_Time","Evening_Rush","Late_Night")

# Create a vector which represents the number of times time slots are to be repeated
Times_rep <- c(4,6,7,5,2)

# Repeat the time slots number of times required and save it as a vector
# The number of elements in this vector should correspond to 24
Time_Slot_final <- rep(Time_Slot,Times_rep)

# create a new dataframe with sequence of number generated and time slots
Time_frame <- data.frame(Time_Slot_final,Request_hour)

#Merge the main uber request dataframe with the new dataframe created
uber_data <- merge(uber_data,Time_frame,by="Request_hour",all.x=TRUE)

#Subset the master dataframe.
#Subsetted dataframe should only consist of Trips completed
Trips_completed <- subset(uber_data,uber_data$Status=="trip completed")

#Plot a bar chart with time-slots on x axis and trips completed on Y-axis
Timeslot_bar <- ggplot(Trips_completed,aes(x=Time_Slot_final))
Trips_completed_Timeslot_plot <- Timeslot_bar+geom_bar(stat="count",col="black",fill="green")+
  ggtitle("Trips completed during different Time Slots")+
  labs(x="Time Slots",y="Trips Completed")+
  geom_text(stat='count',aes(label=..count..),vjust=-0.25)+
  guides(fill=FALSE)+
  scale_x_discrete(limits=c("Morning_Rush","Evening_Rush","Day_Time",
                            "Late_Night","Pre_Morning"))


#view the plot
Trips_completed_Timeslot_plot

#plot a bar chart with time slots on x-axis and request frequency on y-axis
# show the status of requests in different colors, add title, axis labels 
# save the plot as a object
Timeslot_request_count <- ggplot(uber_data,aes(x=factor(Time_Slot_final),fill=factor(Status)))
Trip_request_timeslot_plot <- Timeslot_request_count+geom_bar(stat="count",position = "stack",col="black")+
  ggtitle("Trips during Different Time Slots")+
  scale_x_discrete(limits=c("Evening_Rush","Morning_Rush","Day_Time",
                            "Late_Night","Pre_Morning"))+
  labs(x="Time Slots",y="Number of Requests")+labs(fill="Trip Status")+
  scale_fill_discrete(limits=c("trip completed","no cars available","cancelled"))

#view the plot
Trip_request_timeslot_plot

#problem 1. Large number of service requests got cancelled during the Morning_Rush Time slot
#Subset the Morning Rush time slot data for analysis
Problem1_data <- subset(uber_data,uber_data$Time_Slot_final=="Morning_Rush")

#Plot the bargraph with status of request in x-axis and count in y-axis for Morning rush time slot
#Show the request from different pickup points in different colors
Problem1_count <- ggplot(Problem1_data,aes(x=factor(Status),fill=factor(Pickup.point)))
Cabrush_Morning_plot <- Problem1_count+geom_bar(stat="count",position = "stack")+
  ggtitle("Morning Rush Cab Status")+
  labs(x="Status",y="Total count")+
  labs(fill="Pickup Point")+scale_x_discrete(limits=c("trip completed","cancelled","no cars available"))+
  annotate("text", x=-Inf,y=Inf,label="Airport = 2.80% & City = 97.20%", hjust=-0.05,vjust=0.9)

#view the plot
Cabrush_Morning_plot

#Number of trips cancelled for the Morning rush time slot
Total_trip_cancel <- length(which(Problem1_data$Status=="cancelled"))

#Number of trips cancelled from airport for Morning rush
Airport_trip_cancel <- length(which((Problem1_data$Pickup.point=="airport") & (Problem1_data$Status == "cancelled")))

# Number of trips cancelled from city for Morning rush
City_trip_cancel <- length(which((Problem1_data$Pickup.point=="city") & (Problem1_data$Status == "cancelled")))

# Percentage of trips cancelled from city out of total trips cancelled during morning rush
Percent_trip_cancel_city <- (City_trip_cancel/Total_trip_cancel*100)

# Percentage of trips cancelled from airport out of total trips cancelled during Morning rush
Percent_trip_cancel_airport <- (Airport_trip_cancel/Total_trip_cancel*100)

# Number of trips requested from city to airport during morning rush
Demand_trip_request_city <- length(which(Problem1_data$Pickup.point=="city"))

#Number of trips completed from city to airport during morning rush
Demand_trip_city_completed <- length(which((Problem1_data$Pickup.point=="city")& (Problem1_data$Status=="trip completed")))





# Problem2
# Subset the data for Evening rush from dataframe for analysis
Problem2_data <- subset(subset(uber_data,uber_data$Time_Slot_final=="Evening_Rush"))

# Plot the bar graph with status of requests on x-axis and count in y-axis for evening rush time slot
# Show the request from different pickup points in different colors
Problem2_count <- ggplot(Problem2_data,aes(x=factor(Status),fill=factor(Pickup.point)))
Cabrush_Evening_plot <- Problem2_count+geom_bar(stat="count",position = "stack")+
  ggtitle("Evening Rush Cabs Status")+
  labs(x="Status",y="Total count")+
  labs(fill="Pickup Point")+scale_x_discrete(limits=c("no cars available","trip completed","cancelled"))+
  annotate("text", x=-Inf,y=Inf,label="Airport - 94.90% & City = 5.10%", hjust=-.1,vjust=1)  

# View the plot
Cabrush_Evening_plot

# No of service requests with no cars available for evening rush time slot
Total_nocar_available <- length(which(Problem2_data$Status=="no cars available"))

# No of  service requests with no cars available from airport during evening rush
Airport_nocar_available <- length(which((Problem2_data$Pickup.point=="airport") & (Problem2_data$Status == "no cars available")))

# No of service requests with no cars availablefrom city during evening rush
City_nocar_available <- length(which((Problem2_data$Pickup.point=="city") & (Problem2_data$Status == "no cars available")))

# Percentage of no cars available status from city out of total no cars available during evening rush
Percent_city_nocar <- (City_nocar_available/Total_nocar_available*100)

# Percentage of no cars available status from airport out of total no cars available during evening rush
Percent_airport_nocar <- (Airport_nocar_available/Total_nocar_available*100)

#No of service requests from airport to city during evening rush
Demand_nocar_request_airport <- length(which(Problem2_data$Pickup.point=="airport"))

#No of trips completed from airport to city during evening rush
Demand_nocar_request_airport_completed <- length(which((Problem2_data$Pickup.point=="airport") & (Problem2_data$Status=="trip completed")))

#-----------------------------------End--------------------------------------------------------