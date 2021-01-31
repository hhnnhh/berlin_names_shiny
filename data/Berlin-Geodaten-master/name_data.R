library(dplyr)
library(readr)
getwd()
setwd("C:/Users/Hannah Bohle/Dropbox/R_wissen/namen_berlin_2017")
#list.files("C:/Users/Hannah Bohle/Dropbox/R_wissen/namen_berlin_2017", pattern='\\.csv$')
list.files(pattern = '\\.csv$') # workingdirectory is default

TK<-read.csv2("treptow-koepenick.csv", header= TRUE,encoding="UTF-8")
head(TK)

FK<-read.csv2("friedrichshain-kreuzberg.csv", header= TRUE,encoding="UTF-8")
head(FK)

MH<-read.csv2("marzahn-hellersdorf.csv", header= TRUE,encoding="UTF-8")
head(MH)

L<-read.csv2("lichtenberg.csv", header= TRUE,encoding="UTF-8")
head(L)
M<-read.csv2("mitte.csv", header= TRUE,encoding="UTF-8")
head(M)

L<-read.csv2("lichtenberg.csv", header= TRUE,encoding="UTF-8")
head(L)

N<-read.csv2("neukoelln.csv", header= TRUE,encoding="UTF-8")
head(N)
R<-read.csv2("reinickendorf.csv", header= TRUE,encoding="UTF-8")
head(R)

S<-read.csv2("spandau.csv", header= TRUE,encoding="UTF-8")
head(S)

SZ<-read.csv2("steglitz-zehlendorf.csv", header= TRUE,encoding="UTF-8")
head(SZ)

TS<-read.csv2("tempelhof-schoeneberg.csv", header= TRUE,encoding="UTF-8")
head(TS)

CW<-read.csv2("C:/Users/Hannah Bohle/Dropbox/R_wissen/namen_berlin_2017/charlottenburg-wilmersdorf.csv", header= TRUE,encoding="UTF-8")
head(CW)

P<-read.csv2("C:/Users/Hannah Bohle/Dropbox/R_wissen/namen_berlin_2017/pankow.csv",header=TRUE,encoding="UTF-8")
head(P)
#names17<-rbind(cbind(frame="TK", TK), cbind(frame="FK", FK))
names17<-rbind(cbind(frame="CW",CW), cbind(frame="TK", TK),cbind(frame="P",P),cbind(frame="FK", FK),cbind(frame="MH", MH), cbind(frame="L", L), cbind(frame="N", N), cbind(frame="R", R), cbind(frame="L", L), cbind(frame="SZ", SZ), cbind(frame="TS", TS), cbind(frame="M", M), cbind(frame="S", S))

head(names17)

names(names17)[names(names17)=='frame'] <- 'kiez'
head(names17)
levels(names17$kiez)
is.numeric(names17$anzahl)

#install.packages("tidyr")
library(reshape2)

install.packages("dplyr")
library(dplyr)
#Auswahl nur von den Namen, die einmal im Kiez vergeben wurden
levels(names17$kiez)

t<-table(names17$anzahl,names17$kiez)
mosaicplot(t)
einzig <- filter(names17, anzahl == 1)
head(einzig)

#Nur die Namen, die auch als _Rufnamen_ gegeben wurden (Position 1)
einzigeins<-filter(einzig,position==1)
head(einzigeins)
einzigeins$geschlecht<-NULL
einzigeins$position<-NULL


sum(einzigeins$anzahl) # Anzahl aller einzigartiger Namen in den Kiezen - z.T. aber Berlinweit doppelt vorhanden


#Zusammenfassung über die KIEZE

EE<- reshape(einzigeins,
             idvar = "vorname",
             # unique identifier
             timevar = "kiez",
             # the column represent the timing of the observations
             v.names = c("anzahl"),
             # the columns represent the value of the observation (BP,HR)
             direction = "wide",
             sep = "_"
)
head(EE)


#so bekommt man die Summe der Anzahl der Namen über Berlin
EE$berlin<-rowSums(EE[,2:13],na.rm = TRUE) # summiert die Anzahl aller Namen über die Kieze

head(EE) # jetzt mit Variable EE$berlin, die die Anzahl der Namen über die Kieze enthält
EEE<-filter(EE,berlin==1) # Auswahl nur der Namen und Kieze, die Berlinweit im Jahr 2017 nur einmal vorkommen
head(EEE) # hat funktioniert
sum(EEE$berlin) # wieviele einzigartige Namen gab es 2017 in Berlin? --> 6069! 
EEE$berlin<-NULL # variable no longer needed
summary(EEE)

# Jetzt sind es 6069 - wenn nur die Namen an erster Position 


kreativ<-colSums(EEE[,2:13],na.rm = TRUE) #Die Anzahl über die Kieze summiert und neuer DAtensatz
head(kreativ)
kreativ
names(kreativ)[names(kreativ)=='anzahl_M'] <- 'Mitte'
names(kreativ)[names(kreativ)=='anzahl_P'] <- 'Pankow'
names(kreativ)[names(kreativ)=='anzahl_S'] <- 'Spandau'
names(kreativ)[names(kreativ)=='anzahl_TS'] <- 'Tempelhof-Schöneberg'
names(kreativ)[names(kreativ)=='anzahl_TK'] <- 'Treptow-Köpenick'
names(kreativ)[names(kreativ)=='anzahl_L'] <- 'Lichtenberg'
names(kreativ)[names(kreativ)=='anzahl_FK'] <- 'Friedrichshain-Kreuzberg'
names(kreativ)[names(kreativ)=='anzahl_CW'] <- 'Charlottenburg-Wilmersdorf'
names(kreativ)[names(kreativ)=='anzahl_SZ'] <- 'Steglitz-Zehlendorf'
names(kreativ)[names(kreativ)=='anzahl_N'] <- 'Neukölln'
names(kreativ)[names(kreativ)=='anzahl_MH'] <- 'Marzahn-Hellersdorf'
names(kreativ)[names(kreativ)=='anzahl_R'] <- 'Reinickendorf'
kreativ


########

kreativ<-as.data.frame(kreativ) # numerical array to data frame
is.numeric(kreativ$kreativ) # variable "kreativ" enthält die Anzahl
kreativ$name<-row.names(kreativ) # die Zeilenbezeichnung wird in eigene VAriable umgewandelt
head(kreativ)
names(kreativ)[names(kreativ)=='kreativ'] <- 'anzahl'
is.numeric(kreativ$anzahl)

head(kreativ)
kkreativ<-kreativ[,c(2,1)] # reihenfolge der Kolumnen vertauscht
head(kkreativ)
rownames(kkreativ) <- c() # rownames weg
head(kkreativ)


head(kkreativ) # yep
#names(kreativ)[names(kreativ)=='kreativ'] <- 'c' # rename kreativ to c
#kreativ$c
#head(kreativ)

# merging data with map
berlin_spdf$name  # namen ok?
is.factor(berlin_spdf$name) # namen = factor?
levels(berlin_spdf$name) # namen = levels?

kkreativ$name # namen ok?
is.factor(kkreativ$name)
kkreativ$name<-as.factor(kkreativ$name) # name zu faktor
levels(kkreativ$name) # namen = levels?
is.double(kkreativ$anzahl)
kkreativ$anzahl<-as.integer(kkreativ$anzahl)

berlin_spdf2 <- merge(berlin_spdf, kreativ, by ="name") # data fehlt noch, siehe unten
head(berlin_spdf2)
berlin_spdf2$anzahl
is.numeric(berlin_spdf2$anzahl)
plot(berlin_spdf2$anzahl~berlin_spdf2$name)

hist(berlin_spdf2$anzahl)

is.factor(berlin_spdf2$name)
berlin_spdf2$anzahl
is.numeric(berlin_spdf2$anzahl)
is.integer(berlin_spdf2$anzahl)

#Fehler: Regions defined for each Polygons, Error in Fun(x) object name not found

zz<-ggplot(kreativ, aes(y=anzahl, x=name)) +
  geom_bar(stat="identity")
zz

head(fortify(berlin_spdf2))
ggplot(berlin_spdf2, aes(long, lat)) +
  geom_polygon(aes(group = piece))

berlin_spdf2$cartodb_id
berlin_spdf2$id <-as.numeric(berlin_spdf2$cartodb_id)
berlin_spdf2$id

#install.packages("mapproj")
#library(mapproj)
#g<-ggplot() +
#  geom_polygon(data = berlin_spdf, aes( x = long, y = lat, group = group)) +
#theme_void() +
# coord_map()
#g

#data %>% ggplot( aes(x=E)) + geom_histogram(bins=20, fill='skyblue', color='white') + scale_x_log10()


#ggplot() +
#  geom_polygon(data = berlin_spdf2, aes(fill = name, x = long, y = lat, group = group)) +
# theme_void() +
# coord_map()

berlin <- ggplot(data = berlin_spdf2, aes(x = long, y = lat, group = group))
berlin+geom_path() # plottet die outlines der kieze

# hier an den Margins schrauben?!
berlin + 
  geom_polygon(aes(fill = id)) + # plottet die Kieze als Polygone
  coord_fixed(1.3) +
  guides(fill = FALSE) 


