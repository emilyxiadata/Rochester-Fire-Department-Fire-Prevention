# set some parameters to avoid changing all instances later
year = 2015
span = 5
state = 36
county = 55
tract = "*"

# bring in the gazetteer files of census tracts, which describe their size and location
gaz <- read.table('http://www2.census.gov/geo/docs/maps-data/data/gazetteer/2015_Gazetteer/2015_gaz_tracts_36.txt',
                  header = TRUE)
gaz$GEOID <- as.character(gaz$GEOID)

# filter so that only Monroe County census tracts are included
library(tigris)
tracts_mon <- tracts(state = state, county = county, cb=TRUE)
tracts_roc <- tracts_mon[as.numeric(tracts_mon@data$NAME)<100,]

library(dplyr)
gaz <- filter(gaz, grepl('36055', GEOID))
gaz <- gaz[gaz$ALAND>0,]

# filter so that only Rochester census tracts are included
# this makes the previous filter redundant, so you could actually skip it, but I wanted to demo both methods
gaz <- gaz[gaz$GEOID %in% tracts_roc@data$GEOID,]

# you need to get an API key from http://api.census.gov/data/key_signup.html
library(acs)
api.key.install(key="2176deb12e5dfe8b14b3f0c12d1a15680be489ac")

# to get tabular data with the acs package, you have to create a set of all the geographies
# here we take all the census tracts("*") in Monroe County (55), New York (36)
geo.lookup(state = "NY", county = "Monroe")
mon <- geo.make(state= state,
                county= county, tract= tract)

#B01003_001 Number of people
#B01001_002 Number of male
#B17001_002 Number of people under poverty line
#B02001_002 White alone
#B01001_003 Male under 5
#B01001_020 Male 65 and 66 years
#B01001_021 Male 67 to 69 years
#B01001_022 Male 70 to 74 years
#B01001_023 Male 75 to 79 years
#B01001_024 Male 80 to 84 years
#B01001_025 Male 85 years and over
#B01001_027 Female under 5
#B01001_044 Female 65 and 66 years
#B01001_045 Female 67 to 69 years
#B01001_046 Female 70 to 74 years
#B01001_047 Female 75 to 79 years
#B01001_048 Female 80 to 84 years
#B01001_049 Female 85 years and over
pop <- acs.fetch(endyear = year, span = span, geography = mon, variable = 
                   c("B01003_001","B01001_002","B17001_002","B02001_002","B01001_003","B01001_020","B01001_021","B01001_022",
                     "B01001_023","B01001_024","B01001_025","B01001_027","B01001_044","B01001_045","B01001_046","B01001_047",
                     "B01001_048","B01001_049"))

#B23001_004 Male: 16 to 19 years: In labor force
#B23001_008 Male: 16 to 19 years: In labor force: Civilian: Unemployed
#B23001_011 Male: 20 and 21 years: In labor force
#B23001_015 Male: 20 and 21 years: In labor force: Civilian: Unemployed
#B23001_018 Male: 22 and 24 years: In labor force
#B23001_022 Male: 22 to 24 years: In labor force: Civilian: Unemployed
yemp <- acs.fetch(endyear = year, span = span, geography = mon, variable = 
                   c("B23001_004","B23001_008","B23001_011","B23001_015","B23001_018","B23001_022"))

#B15003_001 Total Number of people over 25 years
#B15003_017 Total Number of people over 25 years Highschool diploma
#B15003_018 Total Number of people over 25 years GED
#B15003_019 Total Number of people over 25 years Less 1 year of college
#B15003_020 Total Number of people over 25 years More 1 year of college
#B15003_021 Total Number of people over 25 years Associates
#B15003_022 Total Number of people over 25 years Bachelors
#B15003_023 Total Number of people over 25 years Masters
#B15003_024 Total Number of people over 25 years Professional School
#B15003_025 Total Number of people over 25 years Doctorate
edu<- acs.fetch(endyear = year, span = span, geography = mon, variable = 
               c("B15003_001","B15003_017","B15003_018","B15003_019","B15003_020","B15003_021",
                 "B15003_022","B15003_023","B15003_024","B15003_025"))

#C18108_005 Number of people under 18 with no disabilities
#C18108_009 Number of people 18 to 64 with no disabilities
#C18108_013 Number of people over 65 with no disabilities
disab <- acs.fetch(endyear = year, span = span, geography = mon, variable = 
                   c("B01003_001","C18108_005","C18108_009","C18108_013"))

#B05009_001 Total Number of Own Children
#B05009_003 Children under 6 living with 2 parents
#B05009_021 Children 6 to 17 living with 2 parents
parental <- acs.fetch(endyear = year, span = span, geography = mon, variable = 
                     c("B05009_001","B05009_003","B05009_021"))

#B23025_002 Total Labor force Over 16
#B23025_005 Unemployed people over 16 in labor force
unemp <- acs.fetch(endyear = year, span = span, geography = mon, variable = 
                     c("B23025_002","B23025_005"))

#B25014_001 Total Number of households
#B25014_005 Owner occupied and 1.01 to 1.50 occupants per room
#B25014_006 Owner occupied and 1.51 to 2.00 occupants per room
#B25014_007 Owner occupied and 2.01 or more occupants per room        
#B25014_011 Renter occupied and 1.01 to 1.50 occupants per room
#B25014_012 Renter occupied and 1.51 to 2.00 occupants per room
#B25014_013 Renter occupied and 2.01 or more occupants per room
crowdedness <- acs.fetch(endyear = year, span = span, geography = mon, variable = 
                           c("B25014_001","B25014_005","B25014_006","B25014_007","B25014_011","B25014_012","B25014_013"))

#B19013_001 Median household income in the past 12 months
income <- acs.fetch(endyear = year, span = span, geography = mon, variable = 
                      "B19013_001")

#B25002_001 Total housing units
#B25002_003 Vacant housing units
vacancy <- acs.fetch(endyear = year, span = span, geography = mon, variable = 
                       c("B25002_001","B25002_003"))

#B25003_001 Total households
#B25003_002 Owner occupied households
owner <- acs.fetch(endyear = year, span = span, geography = mon, variable = 
                     c("B25003_001","B25003_002"))

# now we need to convert to a data frame
pop_roc <- as.data.frame(pop@estimate)
pop_roc$GEOID <- paste(pop@geography$state, "0", pop@geography$county, pop@geography$tract, sep = "")

yemp_roc <- as.data.frame(yemp@estimate)
yemp_roc$GEOID <- paste(yemp@geography$state, "0", yemp@geography$county, yemp@geography$tract, sep = "")

edu_roc <- as.data.frame(edu@estimate)
edu_roc$GEOID <- paste(edu@geography$state, "0", edu@geography$county, edu@geography$tract, sep = "")

parental_roc <- as.data.frame(parental@estimate)
parental_roc$GEOID <- paste(parental@geography$state, "0", parental@geography$county, parental@geography$tract, sep = "")

disab_roc <- as.data.frame(disab@estimate)
disab_roc$GEOID <- paste(disab@geography$state, "0", disab@geography$county, disab@geography$tract, sep = "")

unemp_roc <- as.data.frame(unemp@estimate)
unemp_roc$GEOID <- paste(unemp@geography$state, "0", unemp@geography$county, unemp@geography$tract, sep = "")

crowdedness_roc <- as.data.frame(crowdedness@estimate)
crowdedness_roc$GEOID <- paste(crowdedness@geography$state, "0", crowdedness@geography$county, crowdedness@geography$tract, sep = "")

income_roc <- as.data.frame(income@estimate)
income_roc$GEOID <- paste(income@geography$state, "0", income@geography$county, income@geography$tract, sep = "")

vacancy_roc <- as.data.frame(vacancy@estimate)
vacancy_roc$GEOID <- paste(vacancy@geography$state, "0", vacancy@geography$county, vacancy@geography$tract, sep = "")

owner_roc <- as.data.frame(owner@estimate)
owner_roc$GEOID <- paste(owner@geography$state, "0", owner@geography$county, owner@geography$tract, sep = "")

# we filter to include only Rochester tracts
parental_roc <- parental_roc[parental_roc$GEOID %in% gaz$GEOID,]
disab_roc <- disab_roc[disab_roc$GEOID %in% gaz$GEOID,]
edu_roc <- edu_roc[edu_roc$GEOID %in% gaz$GEOID,]
yemp_roc <- yemp_roc[yemp_roc$GEOID %in% gaz$GEOID,]
pop_roc <- pop_roc[pop_roc$GEOID %in% gaz$GEOID,]
unemp_roc <- unemp_roc[unemp_roc$GEOID %in% gaz$GEOID,]
crowdedness_roc <- crowdedness_roc[crowdedness_roc$GEOID %in% gaz$GEOID,]
income_roc <- income_roc[income_roc$GEOID %in% gaz$GEOID,]
vacancy_roc <- vacancy_roc[vacancy_roc$GEOID %in% gaz$GEOID,]
owner_roc <- owner_roc[owner_roc$GEOID %in% gaz$GEOID,]

# reorder, combine and rename the columns into useful variables
#Population Table (Population, Sex, Poverty, Percentage of Non-white, Percentage of Children, Percentage of Elderly People)
pop_roc <- pop_roc[,c(ncol(pop_roc),1:(ncol(pop_roc)-1))] #Reorder to have GEOID first
pop_roc$male_per <- pop_roc[,3]/pop_roc[,2]
pop_roc$pov_per <- pop_roc[,4]/pop_roc[,2]
pop_roc$nonwhite_per <- (pop_roc[,2]-pop_roc[,5])/pop_roc[,2]
pop_roc$childern <- rowSums(pop_roc[,c(6,13)])
pop_roc$elderly <- rowSums(pop_roc[,c(7,8,9,10,11,12,14,15,16,17,18,19)])
pop_roc$childern_per <- pop_roc[,23]/pop_roc[,2]
pop_roc$elderly_per <- pop_roc[,24]/pop_roc[,2]
pop_roc <- pop_roc[,-c(3:19,23:24)]
names(pop_roc)[2] <- c("pop")

#Male Percentage
male_roc <- data.frame(GEOID = pop_roc$GEOID, male_per = pop_roc$male_per)

#Poverty Percentage
pov_roc <- data.frame(GEOID = pop_roc$GEOID, pov_per = pop_roc$pov_per)

#NonWhite Percentage
NonWhite_roc <- data.frame(GEOID = pop_roc$GEOID, nonwhite_per = pop_roc$nonwhite_per)

#Children Percentage
child_roc <- data.frame(GEOID = pop_roc$GEOID, child_per = pop_roc$childern_per)

#Elderly Percentage
eld_roc <- data.frame(GEOID = pop_roc$GEOID, eld_per = pop_roc$elderly_per)

#Young Employement
yemp_roc <- yemp_roc[,c(ncol(yemp_roc),1:(ncol(yemp_roc)-1))] #Reorder to have GEOID first
yemp_roc$young_laborforce <- rowSums(yemp_roc[,c(2,4,6)])
yemp_roc$young_unemployed <- rowSums(yemp_roc[,c(3,5,7)])
yemp_roc$young_unemployed_rate <- yemp_roc$young_unemployed / yemp_roc$young_laborforce
yemp_roc <- yemp_roc[,-c(2:9)]

#Education
edu_roc <- edu_roc[,c(ncol(edu_roc),1:(ncol(edu_roc)-1))] #Reorder to have GEOID first
edu_roc$HScompleted <- rowSums(edu_roc[,3:11])
edu_roc$HScompleted_rate <- edu_roc$HScompleted / edu_roc[,2]
edu_roc <- edu_roc[,-c(2:12)]

#Disability
disab_roc <- disab_roc[,c(ncol(disab_roc),1:(ncol(disab_roc)-1))] #Reorder to have GEOID first
disab_roc$Nondisabled <- rowSums(disab_roc[,3:5])
disab_roc$Disabled_rate <- 1- disab_roc$Nondisabled / disab_roc[,2]
disab_roc <- disab_roc[,-c(2:6)]

#Parental Presence
parental_roc <- parental_roc[,c(ncol(parental_roc),1:(ncol(parental_roc)-1))] #Reorder to have GEOID first
parental_roc$both_parents <- rowSums(parental_roc[,3:4])
parental_roc$both_parents_rate <- parental_roc$both_parents / parental_roc[,2]
parental_roc <- parental_roc[,-c(2:5)]
parental_roc$both_parents_rate[is.nan(parental_roc$both_parents_rate)] <- 0 

#Unemployment
unemp_roc <- unemp_roc[,c(ncol(unemp_roc),1:(ncol(unemp_roc)-1))] #Reorder to have GEOID first
unemp_roc$unemp_rate <- unemp_roc[,3]/unemp_roc[,2]
unemp_roc <- unemp_roc[,-c(2:3)]

#Crowdedness
crowdedness_roc <- crowdedness_roc[,c(ncol(crowdedness_roc),1:(ncol(crowdedness_roc)-1))] #Reorder to have GEOID first
crowdedness_roc$crowd <- rowSums(crowdedness_roc[,c(3:8)])
crowdedness_roc$crowd_rate <- crowdedness_roc[,9]/crowdedness_roc[,2]
crowdedness_roc <- crowdedness_roc[,-c(2:9)]
crowdedness_roc$crowd_rate[is.nan(crowdedness_roc$crowd_rate)] <- 0 

#Median Household Income
income_roc <- income_roc[,c(ncol(income_roc),1:(ncol(income_roc)-1))] #Reorder to have GEOID first
income_roc <- income_roc[!is.na(income_roc$B19013_001),]

#Vacancy
vacancy_roc <- vacancy_roc[,c(ncol(vacancy_roc),1:(ncol(vacancy_roc)-1))]
vacancy_roc$vacancy_rate <- vacancy_roc[,3]/vacancy_roc[,2]
vacancy_roc <- vacancy_roc[,-c(2,3)]
vacancy_roc$vacancy_rate[is.nan(vacancy_roc$vacancy_rate)] <- 0 

#Owner Occupied
owner_roc <- owner_roc[,c(ncol(owner_roc),1:(ncol(owner_roc)-1))]
owner_roc$owneroccupied_rate <- owner_roc[,3]/owner_roc[,2]
owner_roc <- owner_roc[,-c(2,3)]
owner_roc$owneroccupied_rate[is.nan(owner_roc$owneroccupied_rate)] <- 0

#READ PARCEL DATA
setwd('/Users/lulu/Documents/06 Simon/12 Practicum/NIFRS - Fire Prevention/Data/Parcel')
parcels_coord <- read.csv("ParcelsCoordOnly.csv")

# fix the incorrect variable names (always a good idea to check lat/lon naming)
names(parcels_coord) <- c("SBL20", "LON", "LAT")


library(RANN)
x <- as.data.frame(nn2(gaz[,c("INTPTLONG", "INTPTLAT")], query = parcels_coord[,c("LON","LAT")], k = 2))
x$GEOID1 <- gaz$GEOID[x$nn.idx.1]
x$GEOID2 <- gaz$GEOID[x$nn.idx.2]
x$GEOID <- x$GEOID1
x$prop1 <- 1-(x$nn.dists.1/(x$nn.dists.1+x$nn.dists.2))
x$prop2 <- 1-x$prop1

#Crowdedness
tempDF <- join(x, crowdedness_roc)
names(tempDF)[ncol(tempDF)] <- "VAR1"
tempDF$GEOID <- tempDF$GEOID2
tempDF <- join(tempDF, crowdedness_roc)
names(tempDF)[ncol(tempDF)] <- "VAR2"
parcels_coord$CrowdednessRate <- (tempDF$VAR1*x$prop1)+(tempDF$VAR2*x$prop2)
remove(tempDF)

#Disabled rate
tempDF <- join(x, disab_roc)
names(tempDF)[ncol(tempDF)] <- "VAR1"
tempDF$GEOID <- tempDF$GEOID2
tempDF <- join(tempDF, disab_roc)
names(tempDF)[ncol(tempDF)] <- "VAR2"
parcels_coord$DisabledRate <- (tempDF$VAR1*x$prop1)+(tempDF$VAR2*x$prop2)
remove(tempDF)

#Education rate
tempDF <- join(x, edu_roc) ###Modify this line
names(tempDF)[ncol(tempDF)] <- "VAR1"
tempDF$GEOID <- tempDF$GEOID2
tempDF <- join(tempDF, edu_roc) ###Modify this line
names(tempDF)[ncol(tempDF)] <- "VAR2"
parcels_coord$HSCompletedRate <- (tempDF$VAR1*x$prop1)+(tempDF$VAR2*x$prop2) ###Modify this line
remove(tempDF)

#Both Parents Rate
tempDF <- join(x, parental_roc) ###Modify this line
names(tempDF)[ncol(tempDF)] <- "VAR1"
tempDF$GEOID <- tempDF$GEOID2
tempDF <- join(tempDF, parental_roc) ###Modify this line
names(tempDF)[ncol(tempDF)] <- "VAR2"
parcels_coord$BothParentsRate <- (tempDF$VAR1*x$prop1)+(tempDF$VAR2*x$prop2) ###Modify this line
remove(tempDF)

#Unemployement Rate
tempDF <- join(x, unemp_roc) ###Modify this line
names(tempDF)[ncol(tempDF)] <- "VAR1"
tempDF$GEOID <- tempDF$GEOID2
tempDF <- join(tempDF, unemp_roc) ###Modify this line
names(tempDF)[ncol(tempDF)] <- "VAR2"
parcels_coord$UnemploymentRate <- (tempDF$VAR1*x$prop1)+(tempDF$VAR2*x$prop2) ###Modify this line
remove(tempDF)

#Young Unemployment Rate
tempDF <- join(x, yemp_roc) ###Modify this line
names(tempDF)[ncol(tempDF)] <- "VAR1"
tempDF$GEOID <- tempDF$GEOID2
tempDF <- join(tempDF, yemp_roc) ###Modify this line
names(tempDF)[ncol(tempDF)] <- "VAR2"
parcels_coord$YoungUnemploymentRate <- (tempDF$VAR1*x$prop1)+(tempDF$VAR2*x$prop2) ###Modify this line
remove(tempDF)

#Male Percentage
tempDF <- join(x, male_roc) ###Modify this line
names(tempDF)[ncol(tempDF)] <- "VAR1"
tempDF$GEOID <- tempDF$GEOID2
tempDF <- join(tempDF, male_roc) ###Modify this line
names(tempDF)[ncol(tempDF)] <- "VAR2"
parcels_coord$MalePopRate <- (tempDF$VAR1*x$prop1)+(tempDF$VAR2*x$prop2) ###Modify this line
remove(tempDF)

#Poverty Percentage
tempDF <- join(x, pov_roc) ###Modify this line
names(tempDF)[ncol(tempDF)] <- "VAR1"
tempDF$GEOID <- tempDF$GEOID2
tempDF <- join(tempDF, pov_roc) ###Modify this line
names(tempDF)[ncol(tempDF)] <- "VAR2"
parcels_coord$PovertyRate <- (tempDF$VAR1*x$prop1)+(tempDF$VAR2*x$prop2) ###Modify this line
remove(tempDF)

#NonWhite Percentage
tempDF <- join(x, NonWhite_roc) ###Modify this line
names(tempDF)[ncol(tempDF)] <- "VAR1"
tempDF$GEOID <- tempDF$GEOID2
tempDF <- join(tempDF, NonWhite_roc) ###Modify this line
names(tempDF)[ncol(tempDF)] <- "VAR2"
parcels_coord$NonWhiteRate <- (tempDF$VAR1*x$prop1)+(tempDF$VAR2*x$prop2) ###Modify this line
remove(tempDF)

#Children Percentage
tempDF <- join(x, child_roc) ###Modify this line
names(tempDF)[ncol(tempDF)] <- "VAR1"
tempDF$GEOID <- tempDF$GEOID2
tempDF <- join(tempDF, child_roc) ###Modify this line
names(tempDF)[ncol(tempDF)] <- "VAR2"
parcels_coord$ChildrenRate <- (tempDF$VAR1*x$prop1)+(tempDF$VAR2*x$prop2) ###Modify this line
remove(tempDF)

#Elderly Percentage
tempDF <- join(x, eld_roc) ###Modify this line
names(tempDF)[ncol(tempDF)] <- "VAR1"
tempDF$GEOID <- tempDF$GEOID2
tempDF <- join(tempDF, eld_roc) ###Modify this line
names(tempDF)[ncol(tempDF)] <- "VAR2"
parcels_coord$ElderlyRate <- (tempDF$VAR1*x$prop1)+(tempDF$VAR2*x$prop2) ###Modify this line
remove(tempDF)

#Vacancy Rate
tempDF <- join(x, vacancy_roc) ###Modify this line
names(tempDF)[ncol(tempDF)] <- "VAR1"
tempDF$GEOID <- tempDF$GEOID2
tempDF <- join(tempDF, vacancy_roc) ###Modify this line
names(tempDF)[ncol(tempDF)] <- "VAR2"
parcels_coord$VacancyRate <- (tempDF$VAR1*x$prop1)+(tempDF$VAR2*x$prop2) ###Modify this line
remove(tempDF)

#Owner Occupied Rate
tempDF <- join(x, owner_roc) ###Modify this line
names(tempDF)[ncol(tempDF)] <- "VAR1"
tempDF$GEOID <- tempDF$GEOID2
tempDF <- join(tempDF, owner_roc) ###Modify this line
names(tempDF)[ncol(tempDF)] <- "VAR2"
parcels_coord$OwnerOccupiedRate <- (tempDF$VAR1*x$prop1)+(tempDF$VAR2*x$prop2) ###Modify this line
remove(tempDF)

#Median Income
#Fix Gaz
gaz <- gaz[gaz$GEOID %in% income_roc$GEOID,]
x <- as.data.frame(nn2(gaz[,c("INTPTLONG", "INTPTLAT")], query = parcels_coord[,c("LON","LAT")], k = 2))
x$GEOID1 <- gaz$GEOID[x$nn.idx.1]
x$GEOID2 <- gaz$GEOID[x$nn.idx.2]
x$GEOID <- x$GEOID1
x$prop1 <- 1-(x$nn.dists.1/(x$nn.dists.1+x$nn.dists.2))
x$prop2 <- 1-x$prop1

#Income
tempDF <- join(x, income_roc) ###Modify this line
names(tempDF)[ncol(tempDF)] <- "VAR1"
tempDF$GEOID <- tempDF$GEOID2
tempDF <- join(tempDF, income_roc) ###Modify this line
names(tempDF)[ncol(tempDF)] <- "VAR2"
parcels_coord$MedianIncome <- (tempDF$VAR1*x$prop1)+(tempDF$VAR2*x$prop2) ###Modify this line
remove(tempDF)


#Export Final Data
CensusData <- parcels_coord[,-c(2:3)]
setwd('/Users/lulu/Documents/06 Simon/12 Practicum/NIFRS - Fire Prevention/Data/Output')
write.csv(CensusData, file = 'ParcelCensusData.csv')

