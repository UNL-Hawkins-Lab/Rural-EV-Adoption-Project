library(tidyverse)
library(arrow)
library(readr)
library(AICcmodavg)
library(caret)
library(ggplot2)
#NEED TO SWAP BETWEEN!!!
#Jason's
#exp_data = read_parquet("../EV-Chicken and Egg Project/Data/Transport/Experian Registrations/US_Fleet_County_2021Q4.parquet")
#Paul's
exp_data = read_parquet("Nebraska things/data sets/Experian/US_Fleet_County_2021Q4 (1).parquet")

#took out the massive amount of data unneeded for this because it made my computer crash!
exp_data_reduced<- exp_data %>% select(-one_of('Vehicle Manufacturer', 'Vehicle Make', 'Vehicle Model', 'Vehicle Model Year', 'Vehicle Series', 'Vehicle Door Count', 'Vehicle Drivewheel', 'Vehicle Fuel Type', 'Vehicle Engine Liters', 'Vehicle Engine Cylinders', 'Vehicle Performance Option', 'Vehicle Fuel Delivery', 'Vehicle Wheelbase', 'Vehicle GVW Class', 'Vehicle Weight', 'Vehicle Transmission Type', 'Vehicle Min HP', 'Vehicle Max HP', 'Vehicle Min kW', 'Vehicle Max kW', 'Vehicle Import Domestic HQ', 'Vehicle Import Domestic Sales', 'Fleet Flag', 'Ind/Org Flag')) 


# Combine state/county codes to get a unique id
exp_data_reduced$UIDCTY = ifelse(exp_data_reduced$`County Code`<10,paste(exp_data_reduced$`State Code`,exp_data_reduced$`County Code`,sep="00"),ifelse(exp_data_reduced$`County Code`<100,paste(exp_data_reduced$`State Code`,exp_data_reduced$`County Code`,sep="0"),paste(exp_data_reduced$`State Code`,exp_data_reduced$`County Code`,sep="")))

#had to make the column count as a numeric
exp_data_reduced$GEOID <- as.numeric(as.character(exp_data_reduced$UIDCTY))

#NEED TO SWAP BETWEEN!!!
#Jason's
#NCHSURCodes2013 <- read_csv("NCHSURCodes2013.csv")
#Paul's
NCHSURCodes2013 <- read_csv("Nebraska things/data sets/NCHSURCodes2013.csv")

#getting rid of the extra data here to make computer not explode as much too

NCHSURCodes2013_reduced <- NCHSURCodes2013 %>% select(-one_of('State Abr.', 'County name', 'CBSA title', 'CBSA 2012 pop', 'County 2012 pop', '2006 code', '1990-based code', '...10'))
                                                              
exp_data = exp_data_reduced%>%left_join(NCHSURCodes2013_reduced, by=c("GEOID"="FIPS code"))

# Filter out incomplete vehicles
exp_data_reduced <- exp_data[exp_data$'Vehicle Category'!='Incomplete Vehicle' , ]

#Simplified catergorized of Car,SUV, Truck, or Other!

exp_data_reduced$car_type_simple <- recode(exp_data_reduced$`Vehicle Segmentation`, 'Van - Mini' = "Other",
                                           'Van - Fullsize' = "Other",
                                           'Van - Fullsize Luxury' = "Other",
                                           'Other' = "Other",
                                           'Van - Mini Luxury' = "Other",
                                           'Van - Full Sized' = "Other",'Pickup - Midsize' = "Truck",
                                           'Pickup - Fullsize Luxury' = "Truck",
                                           'Pickup - Fullsize' = "Truck",
                                           'Pickup - Compact' = "Truck",'SUV - Subcompact' = "SUV",
                                           'SUV - Midsize Luxury' = "SUV",
                                           'SUV - Midsize' = "SUV",
                                           'SUV - Large Luxury' = "SUV",
                                           'SUV - Large' = "SUV",
                                           'SUV - Compact' = "SUV",
                                           'CUV - Subcompact' = "SUV",
                                           'CUV - Midsize Luxury' = "SUV",
                                           'CUV - Midsize' = "SUV",
                                           'CUV - Large' = "SUV",
                                           'CUV - Compact Luxury' = "SUV",
                                           'CUV - Compact' = "SUV",
                                           'CUV - Large Luxury' = "SUV",
                                           'SUV - Midsize Exotic' = "SUV",
                                           'CUV - Large Exotic' = "SUV",
                                           'CUV - Compact Exotic' = "SUV",
                                           'SUV - Compact Luxury' = "SUV",
                                           'CUV - Subcompact Exotic' = "SUV",'Sport Car - Subcompact Luxury' = "Car",
                                           'Sport Car - Subcompact Exotic' = "Car",
                                           'Sport Car - Subcompact' = "Car",
                                           'Sport Car - Midsize' = "Car",
                                           'Sport Car - Large Exotic' = "Car",
                                           'Sport Car - Large' = "Car",
                                           'Sport Car - Compact' = "Car",
                                           'Car - Subcompact Luxury' = "Car",
                                           'Car - Subcompact' = "Car",
                                           'Car - Midsize Luxury' = "Car",
                                           'Car - Midsize' = "Car",
                                           'Car - Micro' = "Car",
                                           'Car - Large Luxury' = "Car",
                                           'Car - Large' = "Car",
                                           'Car - Compact Luxury' = "Car",
                                           'Car - Compact' = "Car",
                                           'Sport Car - Midsize Luxury' = "Car",
                                           'Sport Car - Large Luxury' = "Car",
                                           'Car - Compact Exotic' = "Car",
                                           'Sport Car - Compact Luxury' = "Car",
                                           'Car - Large Exotic' = "Car",
                                           'Car - Midsize Exotic' = "Car",
                                           'Sport Car - Compact Exotic' = "Car",
                                           'Sport Car - Midsize Exotic' = "Car",)



#create summary maybe 

exp_data <- exp_data_reduced%>%group_by(`GEOID`,`car_type_simple`)%>%summarise(n=n())


#I have pivoted, aggregated, and joined to create a geoid, total n, and individual vehcile type columns
exp_data_wide <- pivot_wider(exp_data,names_from = car_type_simple, values_from = n)

exp_data_condensed <- aggregate(n ~ GEOID, data=exp_data, sum)

exp_data_pivoted = exp_data_condensed%>%left_join(exp_data_wide, by="GEOID")

#Now I will set up weighted average, but correctly this time

exp_data_pivoted$Car_EV_range <-(exp_data_pivoted$Car*235)
exp_data_pivoted$SUV_EV_range <-(exp_data_pivoted$SUV*264)
exp_data_pivoted$Truck_EV_range <-(exp_data_pivoted$Truck*291)

exp_data_pivoted$mean_EV_range <- ((exp_data_pivoted$Car_EV_range+exp_data_pivoted$SUV_EV_range+exp_data_pivoted$Truck_EV_range)/exp_data_pivoted$n)

mean_county_EV_range <- exp_data_pivoted%>%group_by(`GEOID`,`mean_EV_range`)%>%summarise

#now I'm adding NHTS codes to data to work with

exp_data_reduced$NHTS_code <- recode(exp_data_reduced$`2013 code`,'1'="1",'2'="1",'3'="2",'4'="3",'5'="4",'6'="4")

NCHSURCodes2013_reduced$NHTS_code <- recode(NCHSURCodes2013_reduced$`2013 code`,'1'="1",'2'="1",'3'="2",'4'="3",'5'="4",'6'="4")

mean_county_EV_range_with_NHTS <- mean_county_EV_range%>%left_join(NCHSURCodes2013_reduced, by=c("GEOID"="FIPS code"))

#going to aggregate EV ranges down to each region type

NHTS_totals <- aggregate(mean_EV_range ~ NHTS_code, data=mean_county_EV_range_with_NHTS, mean)






#Opening NHTS trip data

#NEED TO SWAP BETWEEN!!!
#Jason's


#trippub <- read_csv(

  
  #Paul's
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

weighted_trip_distances$weighted_distance <- sum(weighted_trip_distances$WTTRDFIN*weighted_trip_distances$TRPMILES)/sum(weighted_trip_distances$WTTRDFIN)


#going to turn URBANSIZE to NHTS 4 code system
weighted_trip_distances$NHTS_code <- recode(weighted_trip_distances$URBANSIZE,'01'="1",'02'="1",'03'="2",'04'="3",'05'="4",'06'="4")

#sticking expected NHTS style EV ranges to data

Weighted_trips_with_EV <- weighted_trip_distances%>%left_join(NHTS_totals, by=c("NHTS_code"))

#creating percentage with distance/Expected EV range = percent of battery used for trips

Weighted_trips_with_EV$percent_of_trip_satisfied_by_full_EV_charge<-((Weighted_trips_with_EV$weighted_distance/Weighted_trips_with_EV$mean_EV_range))

#creating percentage with distance/Expected EV range = percent of battery used for trips, but not using weighted distance

Weighted_trips_with_EV$percent_trip_divided_EV_unweighted<-((Weighted_trips_with_EV$TRPMILES/Weighted_trips_with_EV$mean_EV_range))

#weighted going to aggregate and find average percent of trips for each NHTS type

NHTS_totals$percent_battery_used_for_trips_weighted<- aggregate(percent_of_trip_satisfied_by_full_EV_charge ~ NHTS_code, data=Weighted_trips_with_EV, mean)

#unweighted distance used going to aggregate and find average percent of trips using for each NHTS type

NHTS_totals$percent_battery_used_for_trips_unweighted<- aggregate(percent_trip_divided_EV_unweighted ~ NHTS_code, data=Weighted_trips_with_EV, mean)

#might have done the Anova correctly, I literally can't tell 

one.way <- aov( percent_of_trip_satisfied_by_full_EV_charge ~ NHTS_code, data = Weighted_trips_with_EV)

summary(one.way)

#Trying to pivot data by regions
pivoted_Weighted_trip_data<- pivot_wider(Weighted_trips_with_EV,names_from = NHTS_code, values_from = percent_trip_divided_EV_unweighted)

NHTS_trip_only <- data.frame(pivoted_Weighted_trip_data$'1',pivoted_Weighted_trip_data$'2',pivoted_Weighted_trip_data$`3`,pivoted_Weighted_trip_data$'4')

#regional % boxplot!
boxplot(NHTS_trip_only, main = "",
        xlab = "Urban-Rural Code", ylab = "% Expected EV battery range spent by trips", 
        names = c("Type 1","Type 2","Type 3","Type 4"))


#trying to create boxplots for each NHTS with the battery% per trips

boxplot(Weighted_trips_with_EV$percent_trip_divided_EV_unweighted, main = "",
        xlab = "All Vehicles", ylab = "% Expected EV battery range spent by trips", 
        names = c("First"))
boxplot(Weighted_trips_with_EV$percent_of_trip_satisfied_by_full_EV_charge, main = "",
        xlab = "All Vehicles", ylab = "% Expected EV battery range spent by trips", 
        names = c("First"))
#means of trip distance by each region
pivoted_trip_distance<- pivot_wider(Weighted_trips_with_EV,names_from = NHTS_code, values_from =TRPMILES)

NHTS_trip_distance_only <- data.frame(pivoted_trip_distance$'1',pivoted_trip_distance$'2',pivoted_trip_distance$`3`,pivoted_trip_distance$'4')
NHTS1<- na.omit(NHTS_trip_distance_only$pivoted_trip_distance..1.)
NHTS2<- na.omit(NHTS_trip_distance_only$pivoted_trip_distance..2.)
NHTS3<- na.omit(NHTS_trip_distance_only$pivoted_trip_distance..3.)
NHTS4<- na.omit(NHTS_trip_distance_only$pivoted_trip_distance..4.)
summary(NHTS1)
summary(NHTS2)
summary(NHTS3)
summary(NHTS4)

#making a one way ANOVA to see if trip distance is statistically significant based on region just cause
one.way2 <- aov( TRPMILES ~ NHTS_code, data = Weighted_trips_with_EV)
summary(one.way2)


#visuals!

mean_county_EV_range_with_NHTS<- na.omit(mean_county_EV_range_with_NHTS)
ggplot(data = mean_county_EV_range_with_NHTS) +  geom_histogram(mapping = aes(x = mean_county_EV_range_with_NHTS$mean_EV_range))+labs(y= "Total Count of Counties With Expected Range", x = "Mean EV Range (miles)")
ggplot(Weighted_trips_with_EV, aes(mean_EV_range, TRPMILES)) + geom_point()
ggplot(Weighted_trips_with_EV, aes(mean_EV_range, TRPMILES)) + geom_jitter(height = 2, width = 2)
ggplot(Weighted_trips_with_EV, aes(mean_EV_range, TRPMILES)) + geom_count()
ggplot(Weighted_trips_with_EV, aes(TRPMILES)) +  geom_dotplot()
ggplot(Weighted_trips_with_EV, aes(TRPMILES)) + geom_area(stat = "bin")
ggplot(Weighted_trips_with_EV, aes(TRPMILES)) + geom_density(kernel = "gaussian")
ggplot(Weighted_trips_with_EV, aes(TRPMILES)) + geom_histogram(binwidth = 1)