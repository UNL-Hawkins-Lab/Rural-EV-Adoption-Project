install.packages("arrow")

library(tidyverse)
library(arrow)
library(readr)

#exp_data = read_parquet("../EV-Chicken and Egg Project/Data/Transport/Experian Registrations/US_Fleet_County_2021Q4.parquet")
exp_data = read_parquet("Nebraska things/data sets/Experian/US_Fleet_County")

# Combine state/county codes to get a unique id
exp_data$UIDCTY = ifelse(exp_data$`County Code`<10,paste(exp_data$`State Code`,exp_data$`County Code`,sep="00"),ifelse(exp_data$`County Code`<100,paste(exp_data$`State Code`,exp_data$`County Code`,sep="0"),paste(exp_data$`State Code`,exp_data$`County Code`,sep="")))

exp_data$GEOID <- as.numeric(as.character(exp_data$UIDCTY))             
# Look at vehicle mix by county type
#NCHSURCodes2013 <- read_csv("NCHSURCodes2013.csv")
NCHSURCodes2013 <- read_csv("Nebraska things/data sets/NCHSURCodes2013.csv")
comb_exp_cty = exp_data%>%left_join(NCHSURCodes2013, by=c("GEOID"="FIPS code"))

exp_data <- comb_exp_cty[comb_exp_cty$'2013 code'=="6" , ]

ct_reg_cty_type_cat = exp_data%>%group_by(`GEOID`,`Vehicle Category`)%>%summarise(n=sum(`Vehicle Count`)) 

# Filter out incomplete vehicles
# With filtered results, % vehicles that are light trucks by county
# How does that percent vary across urban/rural groups - ANOVA analysis within group vs. between groups

# Rural data
exp_data <- comb_exp_cty[comb_exp_cty$'2013 code'=="6" , ]
#creates sum of rural GEOID as identified in 2013 codes
ct_reg_cty_type_cat2 <- aggregate(n ~ GEOID, data=ct_reg_cty_type_cat, sum)
#names it sum of n
ct_reg_cty_type_cat2$n_sum <-ct_reg_cty_type_cat2$n
#joins sum with base rural vehicles type
ct_reg_cty_type_cat_sum <- ct_reg_cty_type_cat%>%left_join(ct_reg_cty_type_cat2, by=c("GEOID"="GEOID"))
#creates percent of each Rural GEOID
ct_reg_cty_type_cat_sum$percent_rural <-((ct_reg_cty_type_cat_sum$n.x/ct_reg_cty_type_cat_sum$n_sum)*100)

#Not Rural data
exp_data_urban <- comb_exp_cty[comb_exp_cty$'2013 code'!="6" , ]
#urban GEOID vehicle type stuffs
ct_reg_cty_type_cat_ur <- exp_data_urban%>%group_by(`GEOID`,`Vehicle Category`)%>%summarise(n=n())
#creates sum of urban GEOID as identified in 2013 codes
ct_reg_cty_type_cat2_ur <- aggregate(n ~ GEOID, data=ct_reg_cty_type_cat_ur, sum)
#names it sum of n
ct_reg_cty_type_cat2_ur$n_sum <-ct_reg_cty_type_cat2_ur$n
#joins sum with base urban vehicles type
ct_reg_cty_type_cat_sum_ur <- ct_reg_cty_type_cat_ur%>%left_join(ct_reg_cty_type_cat2_ur, by=c("GEOID"="GEOID"))
#creates percent of each Rural GEOID
ct_reg_cty_type_cat_sum_ur$percent_urban <-((ct_reg_cty_type_cat_sum_ur$n.x/ct_reg_cty_type_cat_sum_ur$n_sum)*100)

#gonna try to combine Urban and rural % into one data set
#COMPLETELY WRONG ct_reg_cty_type_cat_rural_urban <- merge(ct_reg_cty_type_cat_sum_ur,ct_reg_cty_type_cat_sum,by=c("GEOID"="GEOID"))

#instead of combining the data, just going to stick them onto the og data? my computer might explode
