library(tidyverse)

library(readr)
#perpub <- read_csv("Nebraska things/data sets/NHTS/perpub.csv")
#View(perpub)


hhpub <- read_csv("Nebraska things/data sets/NHTS/hhpub.csv")
view(hhpub)

trippub <- read_csv("Nebraska things/data sets/NHTS/trippub.csv")
View(trippub)


#vehpub <- read_csv("Nebraska things/data sets/NHTS/vehpub.csv")
#View(vehpub)

comb_trip_hh = trippub%>%left_join(hhpub, by=c("HOUSEID"="HOUSEID"))