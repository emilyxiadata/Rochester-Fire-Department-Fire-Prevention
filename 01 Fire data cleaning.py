
import pandas as pd
import geocoder
import numpy as np

#%%#%% Load data

#Parcel Data
ComAccessory = pd.read_csv('commercialaccessory.csv')
ComStructure = pd.read_csv('commercialstructure.csv')
ComUse = pd.read_csv('commercialuse.csv')
ParcelsCoord = pd.read_csv('ParcelsCoordOnly.csv')
parcelwaterbill = pd.read_csv('parcelwaterbill.csv')
ResAccessory = pd.read_csv('residentialaccessory.csv')
ResStructure = pd.read_csv('residentialstructure.csv')
TaxParcel = pd.read_csv('taxparcel.csv', encoding = 'mbcs')

#%%
#NFIRS Data
nfirsfir = pd.read_csv("nfirsfir.csv")
nfirsstr = pd.read_csv("nfirsstr.csv")
apptimes = pd.read_csv("VWNFIRSAPPTIMES.csv")
firemain = pd.read_csv('COR_VWFIREMAIN_scrubbed.csv', encoding = 'mbcs')
insp = pd.read_csv('insp_scrubbed.csv', encoding = 'mbcs')
nfirsmain = pd.read_csv('nfirsmain_scrubbed.csv', encoding = 'mbcs')

#%%Clean App data
App_labels = ['DISPCALLID','NFIRSMAINID','STRNUM','STREET','CITY','ZIP']
apptimesReduced = apptimes.ix[:,App_labels]
apptimesReduced = apptimesReduced.drop_duplicates()

#%% Join FIRE Tables
FullFireDF = pd.merge(nfirsfir, apptimesReduced, on = 'NFIRSMAINID', how= 'inner')
FullFireDF = pd.merge(nfirsstr, FullFireDF, on = 'NFIRSMAINID', how = 'left')

#%% Clean Fire data
Fire_labels = ['NFIRSSTRID','STRNUM','STREET','CITY','ZIP']
FireDFReduced = FullFireDF.ix[:,Fire_labels]
#Remove Duplicates
FireDFReduced = FireDFReduced.drop_duplicates()
#Remove NA
FireDFReduced = FireDFReduced.ix[~FireDFReduced['STREET'].isnull(),:]
FireDFReduced['FIRE'] = 'YES'

#%% Rename FireDF Columns
FireDFReduced = FireDFReduced.rename(columns={'STRNUM':'STREET_NUM','STREET':'STREET_NAME'})             
             

#%% Create a column with the address
FireDFReduced['ADDRESS'] = FireDFReduced.ix[:,'STREET_NUM']+" "+FireDFReduced.ix[:,'STREET_NAME']+" "+FireDFReduced.ix[:,'CITY']+", NY "+FireDFReduced.ix[:,'ZIP']

#%% Merge Fire and Parcel
Fire_Parcel = pd.merge(FireDFReduced, TaxParcel, on = ['STREET_NUM','STREET_NAME'], how = 'left' )

#%% Number of Nulls
sum(Fire_Parcel['PARCELID'].isnull())

#%%Subset of Parcels without problems
FireDFNoProblems = Fire_Parcel.ix[~Fire_Parcel['PARCELID'].isnull(),[0,1,2,3,4,5,6,9]]

#%%Subset of Parcels with problems
FireDFProblems = Fire_Parcel.ix[Fire_Parcel['PARCELID'].isnull(),0:7]
FireDFProblems = FireDFProblems.reset_index(drop=True)
#%%    
address =  FireDFProblems['ADDRESS']
#%%
geolocations = []
for i in address :
    g = geocoder.google(i)
    geolocations.append(g.latlng)
geolocations

#%%
DFLocations = pd.DataFrame(data=geolocations, columns = ['LONG','LAT'])

#%%
FireDFProblemsLoc = pd.concat([FireDFProblems, DFLocations],axis = 1)

#%% Deleting Structures outside City of Rochester
FireDFProblemsLoc = FireDFProblemsLoc.loc[(FireDFProblemsLoc.CITY_x == 'ROC')|(FireDFProblemsLoc.CITY_x == 'ROCHESTER')]
FireDFProblemsLoc = FireDFProblemsLoc.reset_index(drop=True)

#%% Deleting Fires address without street number
FireDFProblemsLoc.ix[np.isnan(FireDFProblemsLoc.ix[:,"STREET_NUM"]),:]

#%%
pd.to_numeric(FireDFProblemsLoc["STREET_NUM"])



#%%
FireDFProblemsLoc.to_csv('FireDFwithProblems.csv')
FireDFNoProblems.to_csv('FireDFNoProblems.csv')








