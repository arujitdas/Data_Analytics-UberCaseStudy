#load necessary libraries
library(stringr)
library(ggplot2)

#read the csv file
uber.data <- read.csv("Uber Request Data.csv", check.names = F, stringsAsFactors = F)

###################################   Data Cleaning and Preparation   ##############################################
#replace the data from "-" to "/" to have the consistency, this will help in using the date time function appropriately
uber.data$`Request timestamp` <- str_replace_all(uber.data$`Request timestamp`,"-","/")

#format the date, hour and minute
RequestDate <- format(as.POSIXct(strptime(uber.data$`Request timestamp`,"%d/%m/%Y %H:%M",tz="")) ,format = "%d/%m/%Y")
RequestHour <- format(as.POSIXct(strptime(uber.data$`Request timestamp`,"%d/%m/%Y %H:%M",tz="")) ,format = "%H")
RequestMinute <- format(as.POSIXct(strptime(uber.data$`Request timestamp`,"%d/%m/%Y %H:%M",tz="")) ,format = "%M")

#Add date, hour and minute columns to the dataset
uber.data$RequestDate <- RequestDate
# convert the requesthour into numeric for further analysis
uber.data$RequestHour <- as.numeric(RequestHour)
uber.data$RequestMinute <- RequestMinute

#replace the data from "-" to "/" to have the consistency, this will help in using the date time function appropriately
uber.data$`Drop timestamp` <- str_replace_all(uber.data$`Drop timestamp`,"-","/")

#format the date, hour and minute
DropDate <- format(as.POSIXct(strptime(uber.data$`Drop timestamp`,"%d/%m/%Y %H:%M",tz="")) ,format = "%d/%m/%Y")
DropHour <- format(as.POSIXct(strptime(uber.data$`Drop timestamp`,"%d/%m/%Y %H:%M",tz="")) ,format = "%H")
DropMinute <- format(as.POSIXct(strptime(uber.data$`Drop timestamp`,"%d/%m/%Y %H:%M",tz="")) ,format = "%M")
#Add date, hour and minute columns to the dataset
uber.data$DropDate <- DropDate
uber.data$DropHour <- DropHour
uber.data$DropMinute <- DropMinute
# remove the redundant columns, since split is already done from the said columns
uber.data <- subset(uber.data, select = c(-5,-6))
#convert into date format
uber.data$RequestDate <- as.Date(uber.data$RequestDate, format = "%d/%m/%Y")
uber.data$DropDate <- as.Date(uber.data$DropDate, format = "%d/%m/%Y")
####################################################################################################################

# segmented analysis - from the hour, categorize the time into time slots
#5 different time slots are created out of the requested hours - Early morning ( midnight to 4 am), 
#morning ( 4 am to 9 am),  Mid day ( 9 am to 4 pm), Evening ( 4 pm to 9 pm) and late night 
#( 9 pm to midnight)

for(i in 1:nrow(uber.data)) {
  if(uber.data[i,"RequestHour"] <=4) {
    uber.data[i,"DaySlot"] <- "Early Morning"
  }else if (uber.data[i,"RequestHour"] > 4 & uber.data[i,"RequestHour"] <=9){
    uber.data[i,"DaySlot"] <- "Morning"
  }else if (uber.data[i,"RequestHour"] > 9 & uber.data[i,"RequestHour"] <=16){
    uber.data[i,"DaySlot"] <- "Mid Day"
  }else if (uber.data[i,"RequestHour"] > 16 & uber.data[i,"RequestHour"] <=21){
    uber.data[i,"DaySlot"] <- "Evening"
  }else{
    uber.data[i,"DaySlot"] <- "Late Night"
  }}


#####################   Data Treatment   ############################################################################
sum(is.na(uber.data$`Driver id`))/nrow(uber.data) * 100
sum(is.na(uber.data$DropDate)) /nrow(uber.data) *100
# it seems that approx. 39% and 58% of data is having NA values, lets treat these
# this is with the assumption that Driver id with 0 implies that the cars are not available
uber.data$`Driver id`[is.na(uber.data$`Driver id`)==T] <- 0
# setting the date to 01/01/1970. This date implies that either the cars are not available
# or driver has cancelled the trip
uber.data$DropDate[is.na(uber.data$DropDate)==T] <- "01/01/1970"
uber.data$DropHour[is.na(uber.data$DropHour)==T] <- ""
#################################################################################################################

########################### Visually identify the most pressing problems for Uber   #############################

# problem 1
# let us create a bar chart with different trip status in x axis and frequency of requests in y axis
# at the same time identify in which routes cancellation and non availability of cars are more
# it is seen that cancellation is more when requested from city  and non availability of cars is more 
# when requested from airport.
ggplot(data= uber.data, aes(x=uber.data$Status, fill = `Pickup point`))+ 
  geom_bar(position = "dodge")+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 8, hjust = 1)) +
  ggtitle("Trip status vs. Frequency") +  xlab("Trip Status") + 
  ylab("Frequency")+
  theme(
    plot.title = element_text(color="green", size=14, face="bold.italic"),
    axis.title.x = element_text(color="blue", size=14, face="bold"),
    axis.title.y = element_text(color="#993333", size=14, face="bold")
  )

# problem 2
# let us create a bar chart with different time slots in x axis and frequency of requests in y axis
# at the same time identify the time slots where demand is more - from city or from airport
# it is seen that demand is more from city during morning time and demand is more from airport
# during evening time. This causes a supply and demand gap at different time slots
ggplot(data= uber.data, aes(x=uber.data$DaySlot, fill = `Pickup point`))+ 
  geom_bar(position = "dodge")+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 8, hjust = 1)) +
  ggtitle("Time Slot vs. Frequency") +  xlab("Time Slot") + 
  ylab("Frequency")+
  theme(
    plot.title = element_text(color="green", size=14, face="bold.italic"),
    axis.title.x = element_text(color="blue", size=14, face="bold"),
    axis.title.y = element_text(color="#993333", size=14, face="bold")
  )


###################################################################################################
# problem 1 - Cancellation of Cars
# Let us create a box plot to visualise the frequency of requests that get cancelled or show 'no cars available'
ggplot(uber.data, aes(x = uber.data$Status, y = as.numeric(uber.data$RequestHour), fill = `Pickup point`)) + 
  geom_boxplot() + theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 8, hjust = 1)) +
  ggtitle("Trip Status vs. Requested Hours") +  xlab("Trip Status") + ylab("Requested Hours")+
  theme(
    plot.title = element_text(color="green", size=14, face="bold.italic"),
    axis.title.x = element_text(color="blue", size=14, face="bold"),
    axis.title.y = element_text(color="#993333", size=14, face="bold")
  )
# Analysis of Problem 1
#Looking closely into the box plot for the 'CANCELED' request, it could be found that approximately 
#between 9 am to 1 pm there are few or no requests from airport to city. If a driver reaches airport between this time, 
#he/she has to wait for long hours before the next passenger can be on-boarded. 

##################################################################################################################

################################################################################# 
# Problem 2 -  Gap between supply and demand / Non availibility of cars   
# below plot gives the details of cab requests raised between different time slot 

ggplot(data= uber.data, aes(x=uber.data$DaySlot, fill = `Pickup point`))+ 
  geom_bar(position = "dodge")+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 8, hjust = 1)) +
  ggtitle("Time Slot vs. Frequency of Requests") +  xlab("Time Slot") + 
  ylab("Frequency/Count of Requests")+
  theme(
    plot.title = element_text(color="green", size=14, face="bold.italic"),
    axis.title.x = element_text(color="blue", size=14, face="bold"),
    axis.title.y = element_text(color="#993333", size=14, face="bold")
  )

#Analysis of Problem 2
#Here we can see that more numbers of requests are raised from city to airport between 5 am to 9 am 
#(morning time slot) and from airport to city between 5 pm to 9 pm (evening slot). Early morning 
#people prefer to travel from city to airport and in late evening people prefer to come back from 
#airport to city

# this is an additional plot to show cab requests made during different time of the day
# this plot is drill down approach for the above plot
ggplot(data= uber.data, aes(x= as.factor(uber.data$RequestHour), fill = `Pickup point`))+ 
  geom_bar()+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 1, size = 8, hjust = 1)) +
  ggtitle("Hours vs. Frequency of Requests") +  xlab("Hours") + 
  ylab("Frequency/Count of Requests")+
  theme(
    plot.title = element_text(color="green", size=14, face="bold.italic"),
    axis.title.x = element_text(color="blue", size=14, face="bold"),
    axis.title.y = element_text(color="#993333", size=14, face="bold")
  )

##################################################################################################################
