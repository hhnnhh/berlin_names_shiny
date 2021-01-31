# show name trend
library(dplyr)
m<-NULL
m<-df %>% 
  select(vorname,year,anzahl) %>% 
  filter(vorname == "Max")%>%
  group_by(year)%>%
  summarise(s=sum(anzahl))
m

if (df$vorname="Max"){
aggregate(df$anzahl, by=list(df$year), FUN=mean)}

my <-aggregate(m$anzahl~m$year,FUN = sum)
mk <-aggregate(m$anzahl~m$Kiez,FUN = sum)

?aggregate
df1 <- df %>% filter(vorname=="Max")
data<-aggregate(anzahl~year,df1,sum)
data$percent<-(data$anzahl/60000)*100
kiezdata <- aggregate(anzahl~Kiez,df1,sum)
plot(anzahl~Kiez,df1)
data$year <- as.Date(data$year)

#trendline
plot(data$anzahl~data$year, type="b" , lwd=3 , col=rgb(0.1,0.7,0.1,0.8) , ylab="frequency per year" , xlab="date" , bty="l" , pch=20 , cex=4)
abline(h=seq(0,100,10) , col="grey", lwd=0.8)



