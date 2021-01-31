library(dplyr)
library(ggplot2)

# Data format problem: umlaut and sonderzeichen are not displayed correctly, 
# if not provided in UTF-8-BOM format. Problem with UTF-8-BOM: the name of the
# first column is changed to ï..vorname and therefore not readable by program.
# Workaround: I inserted a first column named "a", which is not used by the program
# and UTF-8 can mess with it. There must be a more elegant solution, but it
# is difficult to find because the names are using a wide range of foreign
# sonderzeichen.. :(

# read clean copy of data
df <- read.csv("data/berlin.csv")

# insert mock data which can be altered by UTF-8 format mess
df <- cbind(a = 0, df)

# FILTER ALL POSITIONS LARGER THAN (ONLY ONE OCCURENCE) BUT KEEP NAN!!

#what is the range of positions? Position 8 only occurs once and can be deleted
summary(df$position)

# filter position 8 by KEEPING NAN! (all years until 2017)
df <- subset(df, is.na(position)|position <=7)
#write.csv(df,"Berlin_with_year_position_filtered.csv")

# are the columns names okay?
names(df)

# if column name kiez is in small caps, then change it
df<-df %>% 
  rename(
    Kiez = kiez
  )

# if the column name vorname is messed with, change it
df<-df %>% 
  rename(
    vorname = ï..vorname
  )
# now okay?
names(df)

# save copy
write_excel_csv(df, "data/berlin.csv")

# there are some strange "names", such as "Mittelname", "Großvatername"
(1:nrow(df))[df[,1] == "(Mittelname)"]


# check the data type of the data frame df
str(df)

install.packages("rowr")
library(rowr)

 if (length(df$vorname)<20) {
   (sample(df$vorname,20))    
   } else {
     "No names available"
}

sample(df$vorname)
# check data
summary(df$position)
summary(df$anzahl)
levels(df$vorname)
levels(df$geschlecht)
is.factor(df$geschlecht)
df$geschlecht<-as.factor(df$geschlecht)
is.factor(df$geschlecht)
levels(df$geschlecht)
levels(df$vorname)
levels(df$Kiez)
levels(df$year)
summary(df$year)
is.numeric(df$year)
is.character(df$year)
#df$year<-as.numeric(df$year)
is.numeric(df$anzahl)
