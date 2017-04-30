#########################################################
# Developed by Elsayed Hemayed
# for Data Incubator Challenge
# 
# Exploring Red Light Violations in Chicago
# Data: https://data.cityofchicago.org/api/views/spqx-js37/rows.csv?accessType=DOWNLOAD&bom=true
#
#########################################################

library(gtools)
library(data.table)
library(ggmap)

options(digits = 10)

setwd('C:/Downloads/Data/TrafficViolation_Data')

# Analyzing Red_Light_Camera_Violations
# Importing the dataset
df=fread("Red_Light_Camera_Violations.csv", select = c(2,4,5))
df$`VIOLATION DATE`=as.Date(df$`VIOLATION DATE`,"%m/%d/%Y")

#Compute Violation Daily Statistics
dayStat=aggregate(list(Violation=df$VIOLATIONS), by=list(Date=df$`VIOLATION DATE`), FUN=mean)

# plot to see violation pattern across days
plot(dayStat$Violation~as.Date(dayStat$Date,"%d/%m/%y"),type="l",
     xlab="Date",ylab="Violations",main="Mean of Daily Red Light Violations")


df=fread("Red_Light_Camera_Violations.csv", select = c(2,4,5,8,9))
df<-df[!apply(is.na(df),1,any),]

#Camera Statistics
cameraStat=aggregate(list(Violation=df$VIOLATIONS), by=list(lat=df$LATITUDE,lon=df$LONGITUDE), FUN=mean)

#Keep only high violations
cameraStat=cameraStat[cameraStat$Violation>10,]

# Show the top violations in a GIS map
map <- get_map(location = 'Chicago', zoom = 11, maptype = 'roadmap')

mapPoints <- ggmap(map) + geom_point(aes(x = lon, y = lat, size = Violation), data = cameraStat, colour="red", alpha = .5)

mapPoints

