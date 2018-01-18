#Load Data
commercialstructure <- read.csv("commercialstructure.csv")
residentialstructure <- read.csv("residentialstructure.csv")
taxparcel <- read.csv("taxparcel.csv")
ParcelsCoordOnly <- read.csv("ParcelsCoordOnly.csv")
CensusData <- read.csv("ParcelCensusData.csv")
fireincident <- read.csv("FireIncidents.csv")

library(plyr)


### Commercial Data ###

dfCommercial <- commercialstructure

# Delete columns with only NAs
coldata.dfCommercial <- data.frame(col.name = (names(dfCommercial)))
coldata.dfCommercial$col.name <- as.character(coldata.dfCommercial$col.name)
coldata.dfCommercial$class <- sapply(dfCommercial, class)
coldata.dfCommercial['num.unique'] <- apply(dfCommercial, 2, function(x) length(unique(na.omit(x))))
summary(coldata.dfCommercial$num.unique)
coldata.dfCommercial$pop.pct <- sapply(dfCommercial, function(x) round(sum(!is.na(x))/length(x), 5))
arrange(coldata.dfCommercial, pop.pct)
keep <- coldata.dfCommercial[coldata.dfCommercial$pop.pct > 0.3, "col.name"]
dfCommercial.keep <- dfCommercial[,keep]

# NZV 
library(lattice)
library(ggplot2)
library(caret)
nzv <- nzv(dfCommercial.keep, saveMetrics = T)
nzv

#keep only columns that are not near zero variance
nzv$keep <- nzv$nzv==F
nzv
keep <- row.names(nzv[nzv$keep==T,])
dfCommercial.keep <- dfCommercial[,keep]

dfCommercial.keep <- dfCommercial.keep[,-c(1:2)]

#Find duplicates
dfCommercialDuplicates <- dfCommercial.keep[duplicated(dfCommercial.keep$SBL),]

#Aggregate
#Check for NAs
sum(is.na(dfCommercial.keep$YR_BUILT))
dfCommercial.keep <-subset(dfCommercial.keep, dfCommercial.keep$YR_BUILT>1500)

for (i in 1:nrow(dfCommercial.keep)) {
  if (dfCommercial.keep$STORIES[i] == 0 | is.na(dfCommercial.keep$STORIES[i])) 
    dfCommercial.keep$STORIES[i] <- strsplit(strsplit(as.character(dfCommercial.keep$BUILDING_TYPE[i]), split=',')[[1]][2], split = ' ')[[1]][1]
  if (dfCommercial.keep$STORIES[i] =='2-4') dfCommercial.keep$STORIES[i] <-3
  if (dfCommercial.keep$STORIES[i] =='WITH') dfCommercial.keep$STORIES[i] <-1
}
dfCommercial.keep$STORIES <- as.integer(dfCommercial.keep$STORIES)
dfCommercial.keep <- dfCommercial.keep[,-5]

#Aggregate
dfCommercialClean <- aggregate(cbind(YR_BUILT,STORIES,GROSS_SQ_FT) ~ SBL,
                                data = dfCommercial.keep, function(x) {round(mean(x),0)}, na.action = na.pass)

dfCommercialClean2 <- aggregate(OVER_COND ~ SBL,
                                 data = dfCommercial.keep, function(v) {
                                   uniqv <- unique(v)
                                   uniqv[which.max(tabulate(match(v, uniqv)))]
                                 })

levels(dfCommercialClean2$OVER_COND)[1] <- 'NONE'

#Merge Clean Data sets
DFCommercialFinal <- merge(dfCommercialClean, dfCommercialClean2, by = 'SBL' )
DFCommercialFinal$BUILD_AGE <- 2017 - DFCommercialFinal$YR_BUILT
DFCommercialFinal$YR_BUILT <- NULL
DFCommercialFinal <- DFCommercialFinal[DFCommercialFinal$BUILD_AGE >=0,]

#Process and join taxparcel data
colnames(taxparcel)[3] <- 'SBL'
taxparcel <- taxparcel[c('SBL','CLASSCD','CLASSDSCRP','CURRENT_TOTAL_VALUE','CURRENT_LAND_VALUE','LOT_FRONTAGE','LOT_DEPTH')]
taxparcel$CLASSCD <- as.factor(round_any(taxparcel$CLASSCD, 100, floor))
levels(taxparcel$CLASSCD) <- c("Residential","Vacant","Commercial","Rec and ent","Community services","Industrial","Public services","Green")
colnames(taxparcel)[2] <- "CLASS"
taxparcel$LOT_AREA <- (taxparcel$LOT_FRONTAGE+1)*(taxparcel$LOT_DEPTH+1)
taxparcel$LAND_VALUE_SQFT <- taxparcel$CURRENT_TOTAL_VALUE / taxparcel$LOT_AREA
taxparcel$STRUCTURE_VALUE <- taxparcel$CURRENT_TOTAL_VALUE - taxparcel$CURRENT_LAND_VALUE

DFCommercialFinal <- join(DFCommercialFinal, taxparcel, by='SBL', type='left')
DFCommercialFinal$STRUCTURE_VALUE_SQFT <- DFCommercialFinal$STRUCTURE_VALUE / DFCommercialFinal$GROSS_SQ_FT

#Remove unneccessary columns in Census data and Fire data
colnames(CensusData)[2] <- 'SBL'
colnames(fireincident)[4] <- 'SBL'
CensusData <- CensusData[,-1]
fireincident <- fireincident[,-c(1,2)]

#Join Parcel Data with Census data and Fire data and delete NAs
DFCommercialFinal <- join(DFCommercialFinal, CensusData, by = "SBL", type = "left")
DFCommercialFinal <- join(DFCommercialFinal, fireincident, by = "SBL", type = "left")
DFCommercialFinal <- na.omit(DFCommercialFinal)



### Residential Data ###

dfResidential <- residentialstructure

# Delete columns with many NAs
coldata.dfResidential <- data.frame(col.name = (names(dfResidential)))
coldata.dfResidential$col.name <- as.character(coldata.dfResidential$col.name)
coldata.dfResidential$class <- sapply(dfResidential, class)
coldata.dfResidential['num.unique'] <- apply(dfResidential, 2, function(x) length(unique(na.omit(x))))
summary(coldata.dfResidential$num.unique)
coldata.dfResidential$pop.pct <- sapply(dfResidential, function(x) round(sum(!is.na(x))/length(x), 5))
arrange(coldata.dfResidential, pop.pct)
keep <- coldata.dfResidential[coldata.dfResidential$pop.pct > 0.3, "col.name"]
dfResidential.keep <- dfResidential[,keep]

# NZV 
nzv <- nzv(dfResidential.keep, saveMetrics = T)
nzv

#keep only columns that are not near zero variance
nzv$keep <- nzv$nzv==F
nzv
keep <- row.names(nzv[nzv$keep==T,])
dfResidential.keep <- dfResidential[,keep]

#Find duplicates
dfResidentialDuplicates <- dfResidential.keep[duplicated(dfResidential.keep$SBL),]

#Aggregate
#Check for NAs
sum(is.na(dfResidential.keep$BEDS))
sum(is.na(dfResidential.keep$KITCHENS))
sum(is.na(dfResidential.keep$SECOND_FLOOR_AREA))

#Substitute NAs for 0
dfResidential.keep$BEDS[which(is.na(dfResidential.keep$BEDS))] <- 0
dfResidential.keep$KITCHENS[which(is.na(dfResidential.keep$KITCHENS))] <- 0
dfResidential.keep$SECOND_FLOOR_AREA[which(is.na(dfResidential.keep$SECOND_FLOOR_AREA))] <- 0

#Aggregate
dfResidentialClean <- aggregate(cbind(YR_BUILT,STORIES,BEDS,KITCHENS,BATHS,FIRST_FLOOR_AREA,SECOND_FLOOR_AREA,RESFLRAREA) ~ SBL,
                                data = dfResidential.keep, function(x) {round(mean(x),0)}, na.action = na.pass)

dfResidentialClean2 <- aggregate(cbind(OVER_COND,BLDGSTYLE,EXT_WALL,HEAT_TYPE,AIR_COND,FUEL_TYPE) ~ SBL,
                                 data = dfResidential.keep, function(v) {
                                   uniqv <- unique(v)
                                   uniqv[which.max(tabulate(match(v, uniqv)))]
                                 })
dfResidentialClean2$OVER_COND <- as.factor(dfResidentialClean2$OVER_COND) 
levels(dfResidentialClean2$OVER_COND) <- levels(dfResidential.keep$OVER_COND)
dfResidentialClean2$BLDGSTYLE <- as.factor(dfResidentialClean2$BLDGSTYLE) 
levels(dfResidentialClean2$BLDGSTYLE) <- levels(dfResidential.keep$BLDGSTYLE)
dfResidentialClean2$EXT_WALL <- as.factor(dfResidentialClean2$EXT_WALL) 
levels(dfResidentialClean2$EXT_WALL) <- levels(dfResidential.keep$EXT_WALL)
dfResidentialClean2$HEAT_TYPE <- as.factor(dfResidentialClean2$HEAT_TYPE) 
levels(dfResidentialClean2$HEAT_TYPE) <- levels(dfResidential.keep$HEAT_TYPE)
dfResidentialClean2$AIR_COND <- as.factor(dfResidentialClean2$AIR_COND) 
levels(dfResidentialClean2$AIR_COND) <- levels(dfResidential.keep$AIR_COND)
dfResidentialClean2$FUEL_TYPE <- as.factor(dfResidentialClean2$FUEL_TYPE) 
levels(dfResidentialClean2$FUEL_TYPE) <- levels(dfResidential.keep$FUEL_TYPE)

#Change levels of Categoricals variables
levels(dfResidentialClean2$EXT_WALL)[1] <- "NONE"
levels(dfResidentialClean2$HEAT_TYPE)[1] <- "NONE"
levels(dfResidentialClean2$AIR_COND)[1] <- "NO"
levels(dfResidentialClean2$FUEL_TYPE)[1] <- 'NONE'

#Merge Clean Data sets
DFResidentialFinal <- merge(dfResidentialClean, dfResidentialClean2, by = 'SBL' )
DFResidentialFinal$BUILD_AGE <- 2017 - DFResidentialFinal$YR_BUILT
DFResidentialFinal$YR_BUILT <- NULL
DFResidentialFinal <- DFResidentialFinal[DFResidentialFinal$BUILD_AGE >=0,]

#Join taxparcel data and Census data
DFResidentialFinal <- join(DFResidentialFinal, taxparcel, by='SBL', type='left')
DFResidentialFinal$STRUCTURE_VALUE_SQFT <- DFResidentialFinal$STRUCTURE_VALUE / DFResidentialFinal$RESFLRAREA

#Join with Fire data and delete NAs
DFResidentialFinal <- join(DFResidentialFinal, CensusData, by = "SBL", type = "left")
DFResidentialFinal <-join(DFResidentialFinal, fireincident, by = "SBL", type = "left")
DFResidentialFinal <- na.omit(DFResidentialFinal)


##### FURTHUR CLEANING ########################################################
commercial <- DFCommercialFinal
residential <- DFResidentialFinal

#Commercial
#GROSS_SQ_FT
#Remove outliers
commercial <- commercial[commercial$GROSS_SQ_FT != 1,]

#Normalize (LOG)
commercial$GROSS_SQ_FT <- log(commercial$GROSS_SQ_FT)

#Test
boxplot(commercial$GROSS_SQ_FT)

#BUILD_AGE
#Test
boxplot(commercial$BUILD_AGE)

#Remove outliers
commercial <- commercial[commercial$BUILD_AGE != 443,]

#CURRENT_LAND_VALUE
#Test
boxplot(commercial$CURRENT_LAND_VALUE)

#Remove outliers
commercial <- commercial[commercial$CURRENT_LAND_VALUE >100,]
commercial <- commercial[commercial$CURRENT_LAND_VALUE <4000000,]

#LOG
commercial$CURRENT_LAND_VALUE <- log(commercial$CURRENT_LAND_VALUE)

#NOT NECESSARY COLUMNS
commercial$CURRENT_TOTAL_VALUE <- NULL
commercial$LOT_FRONTAGE <- NULL
commercial$LOT_DEPTH <- NULL
commercial$LOT_AREA <- NULL
commercial$LAND_VALUE_SQFT <- NULL
commercial$STRUCTURE_VALUE_SQFT <- NULL

#STRUCTURE_VALUE
#Test
boxplot(commercial$STRUCTURE_VALUE)

#Remove outliers
commercial <- commercial[commercial$STRUCTURE_VALUE != 0,]

#Normalize (LOG)
commercial$STRUCTURE_VALUE <- log(commercial$STRUCTURE_VALUE)


#Residential
#FIRST_FLOOR_AREA
#Remove outliers
residential <- residential[residential$FIRST_FLOOR_AREA != 1,]

#Test
boxplot(residential$RESFLRAREA)

#CURRENT_LAND_VALUE
#Test
boxplot(residential$CURRENT_LAND_VALUE)

#Remove outliers
residential <- residential[residential$CURRENT_LAND_VALUE <160000,]

#LOG
residential$CURRENT_LAND_VALUE <- log(residential$CURRENT_LAND_VALUE)

#NOT NECESSARY COLUMNS
residential$CURRENT_TOTAL_VALUE <- NULL
residential$LOT_FRONTAGE <- NULL
residential$LOT_DEPTH <- NULL
residential$LOT_AREA <- NULL
residential$LAND_VALUE_SQFT <- NULL
residential$STRUCTURE_VALUE_SQFT <- NULL

#STRUCTURE_VALUE
#Test
boxplot(residential$STRUCTURE_VALUE)

#Remove outliers
residential <- residential[residential$STRUCTURE_VALUE != 0,]

#Normalize (LOG)
residential$STRUCTURE_VALUE <- log(residential$STRUCTURE_VALUE)

#Save Final Data
write.csv(commercial, "CommercialDataFinal5pm.csv")
write.csv(residential, "ResidentialDataFinal5pm.csv")