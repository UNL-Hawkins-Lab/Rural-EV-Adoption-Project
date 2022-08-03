#mean(trippub$TRPMILES,na.rm=T)
#mean(trippub$TRVLCMIN,na.rm=T)
#group_by(hhpub, HBHUR)
#summarise(hhpub,HBHUR)
#condensedtrippub<-
#group_by(trippub, URBAN= 04)
#view(condensedtrippub)
#hh_trippub <- merge(hhpub,trippub,by="HOUSEID",HEADER=TRUE)
#view(hh_trippub)
#ggplot(data = hhpub) +  geom_ (mapping = aes(x = URBRUR))
#ggplot(data = trippub) +  geom_bar(mapping = aes(x = TRVLCMIN),)
#outliers <- boxplot(trippub$TRVLCMIN, plot=FALSE)$out
#x<-trippub
#x<- x[-which(x$TRVLCMIN %in% outliers),]
#ggplot(data = x) +  geom_bar(mapping = aes(x = TRVLCMIN),)
#group_by(trippub$URBRUR) %>%
  #summarise_at(vars(pts), list(name = mean))
#split.data.frame(trippub$URBRUR)

#FOR RURAL STUFFS
trippub2 <- trippub[  trippub$URBRUR=="02" , ]
median(trippub2$TRPMILES,na.rm=T)
median(trippub2$TRVLCMIN,na.rm=T)
outliers2 <- boxplot(trippub2$TRVLCMIN, plot=FALSE)$out
x2<-trippub2
x2<- x2[-which(x2$TRVLCMIN %in% outliers2),]
x2<- as.data.frame(scale(x2))

library(caret)

ss <- preProcess(as.data.frame(x2), method=c("range"))

x2 <- predict(ss, as.data.frame(x2))
x2
ggplot(data = x2) +  geom_bar(mapping = aes(x = TRVLCMIN),)

#FOR URBAN STUFFS
trippub3 <- trippub[  trippub$URBRUR=="01" , ]
mean(trippub3$TRPMILES,na.rm=T)
mean(trippub3$TRVLCMIN,na.rm=T)
median(trippub3$TRPMILES,na.rm=T)
median(trippub3$TRVLCMIN,na.rm=T)
outliers3 <- boxplot(trippub3$TRVLCMIN, plot=FALSE)$out
x3<-trippub3
x3<- x3[-which(x3$TRVLCMIN %in% outliers3),]
ggplot(data = x3) +  geom_bar(mapping = aes(x = TRVLCMIN),)