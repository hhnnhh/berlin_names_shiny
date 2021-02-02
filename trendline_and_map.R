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

library(dplyr)

df$position <-NULL
df$geschlecht <-NULL

 # kiezdata<-df %>% 
 #     filter(vorname == "Marie")%>%
 #     select(Kiez,year,anzahl)%>%
 #     group_by(Kiez,year)%>%
 #     mutate(summe=sum(anzahl))%>%
 #   distinct(Kiez,year,summe)
    

# p<-df %>% 
#   select(vorname,Kiez,anzahl)%>%
#   group_by(Kiez)%>%
#   mutate(total=sum(anzahl))%>%
#   mutate(percentage=round(((anzahl/total)*100),digits=2))%>%
#   mutate(rank=round(rank(desc(percentage))))
# p

#calculate sum, total, percentage with year --> year not necessary for map!!
# summedata<-df %>% 
#   select(vorname,Kiez,year,anzahl)%>%
#   group_by(vorname,Kiez,year)%>%
#   mutate(summe=sum(anzahl))%>%
#   distinct(vorname,Kiez,year,summe)%>%
#   group_by(Kiez)%>%
#   mutate(total=sum(summe))%>%
#   mutate(percentage=round(((summe/total)*100),digits=2))%>%
#   mutate(rank=round(rank(desc(summe))))


# omit year
final_df<-df %>% 
  select(vorname,Kiez,anzahl)%>%
  group_by(vorname,Kiez)%>%
  mutate(summe=sum(anzahl))%>%
  distinct(vorname,Kiez,summe)%>%
  group_by(Kiez)%>%
  mutate(total=sum(summe))%>%
  mutate(percentage=round(((summe/total)*100),digits=2))%>%
  mutate(rank=round(rank(desc(summe))))

library(readr)
final_df <- cbind(a = 0, final_df)
names(final_df)
write_excel_csv(final_df, "data/map_df.csv")
  
# finally filter name from "newdata"
namedata<-final_df %>% 
  filter(vorname == "Max")%>%
  select(vorname,Kiez,summe,percentage,rank)

