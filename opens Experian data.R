install.packages("arrow")

library(tidyverse)
library(arrow)
library(readr)

exp_data = read_parquet("Nebraska things/data sets/Experian/US_Fleet_County")

# Combine state/county codes to get a unique id
exp_data$UIDCTY = ifelse(exp_data$`County Code`<10,paste(exp_data$`State Code`,exp_data$`County Code`,"00"),ifelse(exp_data$`County Code`<100,paste(exp_data$`State Code`,exp_data$`County Code`,"0"),paste(exp_data$`State Code`,exp_data$`County Code`,"")))
 
exp_data$GEOID <- as.numeric(as.character(exp_data$GEOID))             
# Look at vehicle mix by county type
NCHSURCodes2013 <- read_csv("Nebraska things/data sets/NCHSURCodes2013.csv")
comb_exp_cty = exp_data%>%left_join(NCHSURCodes2013, by=c("GEOID"="FIPS code"))

exp_data <- comb_exp_cty[comb_exp_cty$'2013 code'=="6" , ]

ct_reg_cty_type_cat = exp_data%>%group_by(`GEOID`,`Vehicle Category`)%>%summarise(n=n()) 
#ct_reg_cty_type_cat = exp_data%>%group_by(`GEOID`,`Vehicle Fuel Type`,`Vehicle Category`)%>%summarise(n=n()) 