#Load Data
taxparcel <- read.csv("taxparcel.csv")
FireDFReduced <- read.delim("FireDFReduced.csv")

#join Data
library(dplyr)
Fire_Parcel <- join(FireDFReduced, taxparcel, by = c('STREET_NUM','STREET_NAME'), type = 'left', match= 'first' )



FireDFReduced$PARCELID <- NA

FireDFReduced$STREET_NUM <- as.character(FireDFReduced$STREET_NUM)
FireDFReduced$STREET_NAME <- as.character(FireDFReduced$STREET_NAME)
taxparcel$STREET_NAME <- as.character(taxparcel$STREET_NAME)
taxparcel$STREET_NUM <- as.character(taxparcel$STREET_NUM)

for (i in 1:nrow(FireDFReduced)) {
  if(length(which(FireDFReduced$STREET_NUM[i] == taxparcel$STREET_NUM & 
                  FireDFReduced$STREET_NAME[i] == taxparcel$STREET_NAME)) != 0)
  {
    FireDFReduced$PARCELID[i] <- taxparcel$PARCELID[which(FireDFReduced$STREET_NUM[i] == taxparcel$STREET_NUM & 
                                                            FireDFReduced$STREET_NAME[i] == taxparcel$STREET_NAME)]
  }else{
    FireDFReduced$PARCELID[i] <- NA
  }
}



taxparcel$LOW_STREET_NUM <- as.numeric(taxparcel$LOW_STREET_NUM)
taxparcel$HIGH_STREET_NUM <- as.numeric(taxparcel$HIGH_STREET_NUM)



for (i in 1:nrow(FireDFReduced)) {
  if(length(which(FireDFReduced$STREET_NUM[i] >= taxparcel$LOW_STREET_NUM & 
                  FireDFReduced$STREET_NUM[i] <= taxparcel$HIGH_STREET_NUM &
                  FireDFReduced$STREET_NAME[i] == taxparcel$STREET_NAME)) != 0)
    {
    FireDFReduced$PARCELID[i] <- taxparcel$PARCELID[which(FireDFReduced$STREET_NUM[i] >= taxparcel$LOW_STREET_NUM & 
                                                          FireDFReduced$STREET_NUM[i] <= taxparcel$HIGH_STREET_NUM &
                                                          FireDFReduced$STREET_NAME[i] == taxparcel$STREET_NAME)]
  }else{
    FireDFReduced$PARCELID[i] <- NA
  }
}

