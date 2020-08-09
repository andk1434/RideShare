#https://data.cityofnewyork.us/Transportation/FHV-Base-Aggregate-Report/2v9c-2k7f/data

ride <- read.csv(file.choose("/Users/Andrew/Desktop/R projects/FHV_Base_Aggregate_Report-2.csv")
)

taxi <- read.csv("https://www1.nyc.gov/assets/tlc/downloads/csv/data_reports_monthly_indicators.csv")
head(taxi)

head(taxi, 20)
head(ride,20)
str(ride)
str(taxi)

library(zoo) 
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)

#Ridehailing trips per Day

ride$Date <- as.yearmon(paste(ride$Year, ride$Month), "%Y %m")


st <- as.Date("2015-1-1")
en <- as.Date("2020-2-28")
ll <- seq(en, st, by = "-1 month")
t<-as.numeric(days_in_month(ll))

ride %>%
  ggplot(aes(x= Date, y= Total.Dispatched.Trips/t, group= Base.Name, color= Base.Name)) +
  geom_line() + 
  scale_y_continuous(labels = scales::comma) +
  labs(color = "Ridehailing Apps") +
  ggtitle("Ridehailing Trips per Day" ) +
  ylab("Total Dispatched Trips")

#Ridehailing market share


ride2 <- ride %>%
  select(Date, Base.Name, Total.Dispatched.Trips) %>%
  arrange(Date) %>%
  spread(key=Base.Name, value=Total.Dispatched.Trips) %>%
  gather(key=Base.Name, value=Total.Dispatched.Trips, -Date) %>%
  replace_na(list(Total.Dispatched.Trips=0)) 

ride.app <- factor(ride2$Base.Name, levels = c("VIA", "LYFT", "UBER"))

ride2 %>%
  ggplot(aes(x= Date, y= Total.Dispatched.Trips, group= ride.app, fill= ride.app)) +
  geom_area(position = "fill", color= "black") +
  scale_y_continuous(labels = scales::percent) +
  labs(color = "Ridehailing Apps") +
  ggtitle("Ridehailing Market Share" ) +
  ylab("Market Share")

#Ridehailing unique vehicles per month

ride %>%
  ggplot (aes(x= Date, y= Unique.Dispatched.Vehicles, group= Base.Name, color= Base.Name)) +
  geom_line() +
  geom_point() +
  labs(color = "Ridehailing Apps") +
  ylab("Unique Dispatched Vehicles ") +
  ggtitle("Ridehailing Unique Dispatched Vehicles per Month")

#Trips per Day

taxi$Date <- as.yearmon(taxi$Month.Year, "%Y-%m")

df <- merge(taxi,ride, by="Date", all= TRUE)

df2 <- df %>%
  filter(License.Class %in% c("Green", "Yellow")) %>%
  select(Date, License.Class, Trips.Per.Day, Base.Name, Total.Dispatched.Trips)

str(df2)

df2$Trips.Per.Day <- gsub(",","",df2$Trips.Per.Day)

df2$Trips.Per.Day <- as.numeric(as.character(df2$Trips.Per.Day))

df2 %>%
  ggplot(aes(x=Date)) +
  geom_line(aes(y= Trips.Per.Day, group= License.Class, color= License.Class)) + 
  geom_line(aes(y= Total.Dispatched.Trips/30, group= Base.Name,  color= Base.Name)) +
  scale_y_continuous(labels = scales::comma) +
  scale_color_discrete(name = "License Class", 
                    breaks=c("Yellow", "Green", "UBER", "LYFT", "VIA"), 
                    label= c("Yellow" = "Yellow Taxi", "Green" = "Green Taxi", "UBER" = "Uber", "LYFT" = "Lyft", "VIA" = "Via")) +
  ggtitle("Trips Per Day") +
  ylab("Trips Per Day")
  
        


  
  


