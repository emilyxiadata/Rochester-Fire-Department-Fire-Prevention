#Load Data
taxparcel <- read.csv("taxparcel.csv")
FireEasyMatch<- read.csv('FireDFNoProblems.csv')
FireNotEasy <- read.csv('FireDFwithProblems.csv')
parcelscoord <- read.csv('ParcelsCoordOnly.csv')

#Deleting Fires without street number
FireNotEasy<- FireNotEasy[(FireNotEasy$STREET_NUM != ''),] 

#Filtering the Structures without latitude
FireNotEasyWithLat <- FireNotEasy[!is.na(FireNotEasy$LAT),]
FireNotEasyWithoutLat <- FireNotEasy[is.na(FireNotEasy$LAT),] #Data need to be cleaned and try to merge it with parcel

#Find the nearest latitude and longitude
library(RANN)
x <- as.data.frame(nn2(parcelscoord[,c("LONG","LAT")], query = FireNotEasyWithLat[,c("LONG", "LAT")], k = 1))
FireNotEasyWithLat$PARCELID <- parcelscoord$SBL20[x$nn.idx]
FireNotEasyWithLat$nndist <- x$nn.dists

library(plyr)
Y <- join(FireNotEasyWithLat, taxparcel, by = 'PARCELID', type = 'left')
colnames(Y)[18] <- c('STREET_NAME1')
Y <- Y[!is.na(Y$STREET_NAME1),]

Y$STREET_NAME1 <- as.character(Y$STREET_NAME1)
Y$STREET_NAME <- as.character(Y$STREET_NAME)

#Measure String Distance
library(stringdist)
Y$STRING_DISTANCE <- stringdist(Y$STREET_NAME1, 
                                 Y$STREET_NAME, 
                                 method = "osa")

YCorrect <- Y[Y$STRING_DISTANCE<=3,]
YtoReview <- Y[Y$STRING_DISTANCE >3 ,] #Data need to be cleaned and try to merge with parcel again

#Delete Unnecessary Columns
FireEasyMatch <- FireEasyMatch[,c('NFIRSSTRID','FIRE','PARCELID')]
YCorrect <- YCorrect[,c('NFIRSSTRID','FIRE','PARCELID')]

#Fire Data Final
FireIncidents <- rbind(FireEasyMatch,YCorrect)
FireIncidents <- join(FireIncidents, taxparcel, by = 'PARCELID', type = 'right')
FireIncidents <- FireIncidents[,c('NFIRSSTRID','FIRE','PARCELID')]
FireIncidents$FIRE <- as.character(FireIncidents$FIRE)
FireIncidents$FIRE[which(is.na(FireIncidents$FIRE))] <- 'NO'

#Save a CSV File with the data
write.csv(FireIncidents, file = "FireIncidents.csv")








