#ggplot(data = trippub) + geom_point(mapping = aes(x= TRPTRANS, y= TRVLCMIN))
#ggplot(data = trippub) +  geom_bar(mapping = aes(x = TRPTRANS))
#ggplot(data = trippub) +  geom_bar(mapping = aes(x = TRVLCMIN))
#ggplot(data = trippub) + geom_point(mapping = aes(x= TRWAITTM, y= TRVLCMIN))
f <- ggplot(trippub)
f+ geom_boxplot(mapping = aes(x= TRPTRANS, y= TRVLCMIN))
