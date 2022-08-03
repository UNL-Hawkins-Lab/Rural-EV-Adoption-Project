library(tidyverse)
library(arrow)
library(readr)


#INSERT TRIPPUB DATA BELOW
trippub <- read_csv("Nebraska things/data sets/NHTS/trippub.csv")

#had some data where skipped and not ascertained in vehicle id that had to go
trippub2 <-trippub[!(trippub$VEHID=='-1' | trippub$VEHID=='-9'),]
#unique id for each vehicle in households
trippub2$uniqueid <- paste(trippub2$HOUSEID,trippub2$VEHID, sep="")

#Removing walkers,weird little vehicles like segways, bikes, non-response weird stuffs, and all non ground vehicles from data
trippub_drivers_only <- trippub2[trippub2$DRIVER=="01",]

#condensing the data for each vehicle to find distance traveled for each vehicle
trippub_drivers_only$uniqueid <- as.numeric(as.character(trippub_drivers_only$uniqueid))

# not ascertained trips in miles had to go 
trippub_drivers_only <-trippub_drivers_only[!(trippub_drivers_only$TRPMILES=='-9'),]

#trip lengths by unique house-vehicle id
trip_length_for_each_vehicle<-aggregate(TRPMILES ~ uniqueid, data=trippub_drivers_only, sum)

#creating rural and urban columns to stick with trip lengths
urban_or_not <-trippub_drivers_only%>%group_by(`uniqueid`,`URBRUR`,)%>%summarise
classes <-trippub_drivers_only%>%group_by(`uniqueid`,`URBANSIZE`,)%>%summarise
urban_or_not_classes<- classes%>%left_join(urban_or_not, by=c("uniqueid"))

#finshed product with all the urban and rural classifications
trip_miles_urban_or_not <- trip_length_for_each_vehicle%>%left_join(urban_or_not_classes, by=c("uniqueid"))

#removes outliers from data 

outliers <- boxplot(trip_miles_urban_or_not$TRPMILES, plot=FALSE)$out
x2<-trip_miles_urban_or_not
x2<- x2[-which(x2$TRPMILES %in% outliers),]

#getting the n for all the weights 
weights <-trippub_drivers_only%>%group_by(`uniqueid`,`WTTRDFIN`,)%>%summarise(n=n())

# sticking weights on with the trip distances
weighted_trip_distances <- x2%>%left_join(weights, by=c("uniqueid"))

#creating the weighted distances

weighted_trip_distances$weighted_distance <- (weighted_trip_distances$WTTRDFIN*weighted_trip_distances$TRPMILES)/sum(weighted_trip_distances$n)