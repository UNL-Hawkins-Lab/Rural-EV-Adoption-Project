

library(tidyverse)
library(arrow)
library(readr)

#NEED TO SWAP BETWEEN!!!
#Jason's
exp_data = read_parquet("../EV-Chicken and Egg Project/Data/Transport/Experian Registrations/US_Fleet_County_2021Q4.parquet")
#Paul's
#exp_data = read_parquet("Nebraska things/data sets/Experian/US_Fleet_County_2021Q4 (1).parquet")

# Combine state/county codes to get a unique id
exp_data$UIDCTY = ifelse(exp_data$`County Code`<10,paste(exp_data$`State Code`,exp_data$`County Code`,sep="00"),ifelse(exp_data$`County Code`<100,paste(exp_data$`State Code`,exp_data$`County Code`,sep="0"),paste(exp_data$`State Code`,exp_data$`County Code`,sep="")))

exp_data$GEOID <- as.numeric(as.character(exp_data$UIDCTY))             
# Look at vehicle mix by county type

#NEED TO SWAP BETWEEN!!!
#Jason's
NCHSURCodes2013 <- read_csv("NCHSURCodes2013.csv")
#Paul's
#NCHSURCodes2013 <- read_csv("Nebraska things/data sets/NCHSURCodes2013.csv")

comb_exp_cty = exp_data%>%left_join(NCHSURCodes2013, by=c("GEOID"="FIPS code"))

# Filter out incomplete vehicles
exp_data <- comb_exp_cty[comb_exp_cty$'Vehicle Category'!='Incomplete Vehicle' , ]
ct_reg_cty_type_cat = exp_data%>%group_by(`GEOID`,`Vehicle Category`)%>%summarise(n=n())
# With filtered results, % vehicles that are light trucks by county
ct_reg_cty_type_cat2 <- aggregate(n ~ GEOID, data=ct_reg_cty_type_cat, sum)
ct_reg_cty_type_cat2$total_vehicles <-ct_reg_cty_type_cat2$n
ct_reg_cty_type_cat_sum <- ct_reg_cty_type_cat%>%left_join(ct_reg_cty_type_cat2, by=c("GEOID"="GEOID"))
ct_reg_cty_type_cat_sum$percent_total <-((ct_reg_cty_type_cat_sum$n.x/ct_reg_cty_type_cat_sum$n_sum)*100)
ct_reg_cty_type_cat_sum_truck <- ct_reg_cty_type_cat_sum[ct_reg_cty_type_cat_sum$'Vehicle Category'=='Light Truck' , ]

# How does that percent vary across urban/rural groups - ANOVA analysis within group vs. between groups
ct_reg_cty_type_cat_sum_truck$percentages<-((ct_reg_cty_type_cat_sum_truck$n.x/ct_reg_cty_type_cat_sum_truck$total_vehicles)*100)
 
trucks_percent_with_codes = ct_reg_cty_type_cat_sum_truck%>%left_join(NCHSURCodes2013, by=c("GEOID"="FIPS code"))

#added NHTS Data code version as well

trucks_percent_with_codes$NHTS_version <- recode(trucks_percent_with_codes$`2013 code`,'1'="1",'2'="1",'3'="2",'4'="3",'5'="4",'6'="4")

view(trucks_percent_with_codes)