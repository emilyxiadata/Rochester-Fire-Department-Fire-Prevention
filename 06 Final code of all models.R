commercial <- read.csv('CommercialDataFinal5pm.csv')
residential <- read.csv('ResidentialDataFinal5pm.csv')

#Libraries
library(tree)
library(randomForest)
library(gbm)
library(e1071)

#Useful Functions
accuracy = function(cm){  # input confusion matrix
  return(sum(diag(cm))/sum(cm))  # accuracy
}

tpr = function(cm){
  return(cm[2,2]/sum(cm[,2]))
}

predper = function(cm){
  return(sum(cm[2,])/sum(cm))
}

fpr = function(cm){
  return(cm[2,1]/sum(cm[,1]))
}

fnr = function(cm){
  return(cm[1,2]/sum(cm[,2]))
}

#Clean Data
commercialDF <- commercial[,-c(1,2,3,9)]
residentialDF <- residential[,-c(1,2,3,18)]

#Train and Test Data sets
set.seed (2)
trainC=sample(1:nrow(commercialDF), 0.6*nrow(commercialDF))
commercialDFTest <- commercialDF[-trainC,]
commercialFireTest <- commercialDF[-trainC, "FIRE"]

trainR <- sample(1:nrow(residentialDF), 0.6*nrow(residentialDF))
residentialDFTest <- residentialDF[-trainR,]
residentialFireTest <- residentialDF[-trainR, "FIRE"]

##############################################################################
#Classification Tree##########################################################
##############################################################################
#Commercial Data
tree.commercial <- tree(FIRE~. , data = commercialDF )
summary(tree.commercial)

plot(tree.commercial)
text(tree.commercial, pretty=0)

#Tree Accuracy
tree.commercial <- tree(FIRE~. ,data = commercialDF ,subset =trainC )
tree.commercial.pred <- predict(tree.commercial ,commercialDFTest ,type ="class")
table(tree.commercial.pred ,commercialFireTest)

#Accuracy
(1924+140)/length(trainC)

#Prune tree
set.seed (3)
cv.commercialtree <- cv.tree(tree.commercial ,FUN=prune.misclass)
cv.commercialtree$size
cv.commercialtree$dev
cv.commercialtree$k
cv.commercialtree$method

prune.commercialtree =prune.misclass(tree.commercial, best =4)
plot(prune.commercialtree)
text(prune.commercialtree, pretty=0)

#Tree Accuracy
tree.commercial.pred <- predict(prune.commercialtree ,commercialDFTest ,type ="class")
cm.tree.commercial <- table(tree.commercial.pred ,commercialFireTest)

#Accuracy
accuracy(cm.tree.commercial)
tpr(cm.tree.commercial)
fpr(cm.tree.commercial)
predper(cm.tree.commercial)

########################################################################################
#Residential
tree.residential <- tree(FIRE~ . , data = residentialDF )
summary(tree.residential)

plot(tree.residential)
text(tree.residential, pretty=0)

#Tree Accuracy
tree.residential <- tree(FIRE~. ,data = residentialDF ,subset =trainR)
tree.residential.pred <- predict(tree.residential ,residentialDFTest ,type ="class")
cm.tree.residential <- table(tree.residential.pred ,residentialFireTest)

#Accuracy
accuracy(cm.tree.residential)
tpr(cm.tree.residential)
fpr(cm.tree.residential)
predper(cm.tree.residential)


###########################################################################################
#Bagging###################################################################################
###########################################################################################
#commercialDF$FIRE <- ifelse(commercialDF$FIRE == 0, "NO","YES")
#commercialDF$FIRE <- as.factor(commercialDF$FIRE)
#Commercial
bag.commercial <- randomForest(FIRE~.,data=commercialDF ,subset =trainC ,mtry=21, importance =TRUE, cutoff= c(0.55,0.45))
bag.commercial

#Bagging Accuracy
bag.commercial.pred <- predict(bag.commercial ,commercialDFTest ,type ="class")
cm.bag.commercial <- table(bag.commercial.pred ,commercialFireTest)

accuracy(cm.bag.commercial)
tpr(cm.bag.commercial)
fpr(cm.bag.commercial)
fnr(cm.bag.commercial)
predper(cm.bag.commercial)

bag.commercial.predP <- predict(bag.commercial ,commercialDFTest ,type ="prob")
plot(ecdf(bag.commercial.predP[2516:5030]))

#Commercial 2
bag.commercial2 <- randomForest(FIRE~.,data=commercialDF ,subset =trainC ,mtry=21, importance =TRUE, cutoff= c(0.65,0.35))

#Bagging Accuracy
bag.commercial.pred2 <- predict(bag.commercial2 ,commercialDFTest ,type ="class")
cm.bag.commercial2 <- table(bag.commercial.pred2 ,commercialFireTest)


############################################################################################
#residentialDF$FIRE <- ifelse(residentialDF$FIRE == 0, "NO","YES")
#residentialDF$FIRE <- as.factor(residentialDF$FIRE)
#Residential
bag.residential <- randomForest(FIRE~.,data=residentialDF ,subset =trainR ,mtry=31, importance =TRUE, cutoff = c(0.84,0.16))
bag.residential

#Bagging Accuracy
bag.residential.pred <- predict(bag.residential ,residentialDFTest ,type ="class")
cm.bag.residential <- table(bag.residential.pred ,residentialFireTest)

accuracy(cm.bag.residential)
tpr(cm.bag.residential)
fpr(cm.bag.residential)
predper(cm.bag.residential)

#Probabilities
bag.residential.predP <- predict(bag.residential ,residentialDFTest ,type ="prob")
plot(ecdf(bag.residential.predP[21082:42162]))

#Residential 2
bag.residential2 <- randomForest(FIRE~.,data=residentialDF ,subset =trainR ,mtry=31, importance =TRUE, cutoff= c(0.88,0.12))

#Bagging Accuracy
bag.residential.pred2 <- predict(bag.residential2 ,residentialDFTest ,type ="class")
cm.bag.residential2 <- table(bag.residential.pred2 ,residentialFireTest)


###########################################################################################
#Random Forest#############################################################################
###########################################################################################
#commercialDF$FIRE <- ifelse(commercialDF$FIRE == 0, "NO","YES")
#commercialDF$FIRE <- as.factor(commercialDF$FIRE)
#Commercial
rf.commercial <- randomForest(FIRE~.,data=commercialDF ,subset =trainC , importance =TRUE, cutoff = c(0.55, 0.45))
rf.commercial

#Random Forest Accuracy
rf.commercial.pred <- predict(rf.commercial ,commercialDFTest ,type ="class")
cm.rf.commercial <- table(rf.commercial.pred ,commercialFireTest)

cm.rf.commercial
accuracy(cm.rf.commercial)
tpr(cm.rf.commercial)
fpr(cm.rf.commercial)
fnr(cm.rf.commercial)
predper(cm.rf.commercial)

rf.commercial.predP <- predict(rf.commercial ,commercialDFTest ,type ="prob")
plot(ecdf(rf.commercial.predP[2516:5030]))

#Commercial 2
rf.commercial2 <- randomForest(FIRE~.,data=commercialDF ,subset =trainC , importance =TRUE, cutoff= c(0.66,0.34))

#RF Accuracy
rf.commercial.pred2 <- predict(rf.commercial2 ,commercialDFTest ,type ="class")
cm.rf.commercial2 <- table(rf.commercial.pred2 ,commercialFireTest)

predper(cm.rf.commercial2)

############################################################################################
#Residential
rf.residential <- randomForest(FIRE~.,data=residentialDF ,subset =trainR , importance =TRUE, cutoff = c(0.85, 0.15))
rf.residential

#Random Forest Accuracy
rf.residential.pred <- predict(rf.residential ,residentialDFTest ,type ="class")
cm.rf.residential <- table(rf.residential.pred ,residentialFireTest)

cm.rf.residential
accuracy(cm.rf.residential)
tpr(cm.rf.residential)
fpr(cm.rf.residential)
predper(cm.rf.residential)

#Probabilities
rf.residential.predP <- predict(rf.residential ,residentialDFTest ,type ="prob")
plot(ecdf(rf.residential.predP[21082:42162]))

#Residential 2
rf.residential2 <- randomForest(FIRE~.,data=residentialDF ,subset =trainR, importance =TRUE, cutoff= c(0.885,0.115))

#Random Forest Accuracy
rf.residential.pred2 <- predict(rf.residential2 ,residentialDFTest ,type ="class")
cm.rf.residential2 <- table(rf.residential.pred2 ,residentialFireTest)




###########################################################################################
#Boosting##################################################################################
###########################################################################################
#Commercial
commercialDF$FIRE <- ifelse(commercialDF$FIRE == "NO", 0,1)
boosting.commercial <- gbm(FIRE~.,data=commercialDF[trainC,] , distribution = "bernoulli", n.trees = 5000, interaction.depth = 2, shrinkage =0.2)
summary(boosting.commercial)

#Boosting Accuracy
boosting.commercial.pred <- predict(boosting.commercial ,newdata = commercialDFTest , n.trees = 5000, type = "response")
plot(ecdf(boosting.commercial.pred))

cm.boost.commercial <- table(ifelse(boosting.commercial.pred>0.65, "YES","NO") ,commercialFireTest)

cm.boost.commercial
accuracy(cm.boost.commercial)
tpr(cm.boost.commercial)
fpr(cm.boost.commercial)
predper(cm.boost.commercial)

#Commercial 2
cm.boost.commercial2 <- table(ifelse(boosting.commercial.pred>0.35, "YES","NO") ,commercialFireTest)

#Boosting Accuracy
cm.boost.commercial2
accuracy(cm.boost.commercial2)
tpr(cm.boost.commercial2)
fpr(cm.boost.commercial2)
fnr(cm.boost.commercial2)
predper(cm.boost.commercial2)


############################################################################################
#Residential
residentialDF$FIRE <- ifelse(residentialDF$FIRE == "NO", 0,1)
boosting.residential <- gbm(FIRE~.,data=residentialDF[trainR,] , distribution = "bernoulli", n.trees = 5000, interaction.depth = 2, shrinkage =0.2)
summary(boosting.residential)

#Boosting Accuracy
boosting.residential.pred <- predict(boosting.residential ,newdata = residentialDFTest, n.trees = 5000, type = "response")
plot(ecdf(boosting.residential.pred))

cm.boost.residential <- table(ifelse(boosting.residential.pred>0.11, "YES","NO") ,residentialFireTest)

cm.boost.residential
accuracy(cm.boost.residential)
tpr(cm.boost.residential)
fpr(cm.boost.residential)
predper(cm.boost.residential2)

#Residential 2
cm.boost.residential2 <- table(ifelse(boosting.residential.pred>0.073, "YES","NO") ,residentialFireTest)



##############################################################################
#Support Vector Machine##########################################################
##############################################################################
library(e1071)
library(ROCR)

commercial <- read.csv('CommercialDataFinal5pm.csv')
residential <- read.csv('ResidentialDataFinal5pm.csv')

commercialDF <- commercial[,-c(1,2,3,9)]
residentialDF <- ResidentialDataFinal[,-c(1,2,3,18)] 

commercialDF$FIRE<- ifelse(commercialDF$FIRE == "YES",1,0)
commercialDF$FIRE <- as.factor(commercialDF$FIRE)
residentialDF$FIRE<- ifelse(residentialDF$FIRE == "YES",1,0)
residentialDF$FIRE <- as.factor(residentialDF$FIRE)

## Commercial ##
set.seed(99)
train = sample(1:nrow(commercialDF),nrow(commercialDF)*0.6) 

Ctrain = commercialDF[train,]  
Ctest = commercialDF[-train,]

X.Ctrain = Ctrain[,-22]
Y.Ctrain = Ctrain$FIRE
X.Ctest = Ctest[,-22]
Y.Ctest = Ctest$FIRE

fit2 = svm(FIRE~., data = Ctrain, kernel='radial', gamma=0.9, cost=50, probability=TRUE)
pred2 <- predict(fit2, X.Ctest, probability = TRUE)
pred2 <- as.data.frame(attr(pred2,"probabilities"))[,'1']

#Set the threshold to be 0.33 to get 13.56% predicted positive
predictions2 = pred2
predictions2[pred2 >= .33] <- 1
predictions2[pred2 < .33] <- 0

cm2 = table(predictions2, Y.Ctest)
#              Y.Ctest
#predictions2    0    1
#           0 1933  241
#           1  132  209

accuracy(cm2) #0.8516899
tpr(cm2) #0.4644444
fpr(cm2) #0.06392252
fnr(cm2) #0.5355556
predpositive(cm2) #0.1355865

#ROC curve
predScore2 <- prediction(predictions2, Y.Ctest)
perf2 <- performance(predScore2, measure = "tpr", x.measure = "fpr")
plot(perf2, lwd=2, main="ROC Curve")

# AUC 
auc.tmp <- performance(predScore2, "auc")
auc2 <- as.numeric(auc.tmp@y.values)  #0.700261

#Set the threshold to be 0.22 to get 20.2% predicted positive
predictions21 = pred2
predictions21[pred2 >= .22] <- 1
predictions21[pred2 < .22] <- 0
cm21 = table(predictions21, Y.Ctest)
accuracy(cm21) #0.805169
tpr(cm21) #0.52
fpr(cm21) #0.1326877
fnr(cm21) #0.48
predpositive(cm21) #0.2019881

#Set the threshold to be 0.152 to get 50% predicted positive
predictions22 = pred2
predictions22[pred2 >= .152] <- 1
predictions22[pred2 < .152] <- 0
cm22 = table(predictions22, Y.Ctest)
accuracy(cm22) #0.5713718
tpr(cm22) #0.7022222
fpr(cm22) #0.4571429
fnr(cm22) #0.2977778
predpositive(cm22) #0.500994


## Residential ##
set.seed(99)
train = sample(1:nrow(residentialDF),nrow(residentialDF)*0.6) 

Rtrain = residentialDF[train,]  
Rtest = residentialDF[-train,]

X.Rtrain = Rtrain[,-24]
Y.Rtrain = Rtrain$FIRE
X.Rtest = Rtest[,-24]
Y.Rtest = Rtest$FIRE

fit3 <- svm(FIRE ~ ., data=Rtrain, kernel='radial', gamma=0.9, cost=50, probability=TRUE)
pred3 <- predict(fit3, X.Rtest, probability=TRUE)
pred3<-as.data.frame(attr(pred3,"probabilities"))[,'1']

#Set the threshold to be 0.085 to get 13.1% predicted positive
predictions3 = pred3
predictions3[pred3 >= .085] <- 1
predictions3[pred3 < .085] <- 0

cm3 = table(predictions3, Y.Rtest)
#                 Y.Rtest
#predictions3     0     1
#           0 17353   959
#           1  2389   380

accuracy(cm3) #0.841184
tpr(cm3) #0.2837939
fpr(cm3) #0.121011
fnr(cm3) #0.7162061
predpositive(cm3) #0.1313505

#Set the threshold to be 0.075 to get 20.6% predicted positive
predictions31 = pred3
predictions31[pred3 >= .075] <- 1
predictions31[pred3 < .075] <- 0

cm31 = table(predictions31, Y.Rtest)
#                  Y.Rtest
#predictions31     0     1
#            0 15887   848
#            1  3855   491

accuracy(cm31) #0.7769081
tpr(cm31) #0.7769081
fpr(cm31) #0.195269
fnr(cm31) #0.6333084
predpositive(cm31) #0.2061572

#ROC curve
predScore3 <- prediction(predictions3, Y.Rtest)
perf3 <- performance(predScore3, measure = "tpr", x.measure = "fpr")
plot(perf3, lwd=2, main="ROC Curve")

# AUC 
auc.tmp <- performance(predScore3, "auc")
auc3 <- as.numeric(auc.tmp@y.values)  #0.5857113



##############################################################################
#Naive Bayes##########################################################
##############################################################################

#######Commercial###########

data1 <- read.csv(file.choose(), header = TRUE, sep = ",")
head(data1)

commercialData <- data1
head(commercialData)

commercialData$STORIES <- as.factor(commercialData$STORIES)


#========Discretization using summary() ==========#
# The following groupings within the features closely utilized the data point quantile distribution of the full dataset

# First check the summary for of the features.
summary(commercialData[ ,c("GROSS_SQ_FT", "BUILD_AGE", "CLASS", "CURRENT_LAND_VALUE", "STRUCTURE_VALUE" )])
# Try "GROSS_SQ_FT" breaks 0, 2860, 4520, 9780, 939080
# Try "BUILD_AGE" breaks 0, 57, 87, 443
# Try "CLASSCD" breaks 411, 484, 963
# Try "CURRENT_TOTAL_VALUE" breaks 0, 88300,166500,400000,5000000, 50000000, 89388500 
commercialData$GROSS_SQ_FT = cut(commercialData$GROSS_SQ_FT, breaks=c(4, 7.961, 8.415, 9.185, 13.753), labels=c("xs","s","m","l"))
commercialData$BUILD_AGE = cut(commercialData$BUILD_AGE, breaks=c(-1, 57, 87, 443), labels=c("s","m","l"))
commercialData$CURRENT_LAND_VALUE = cut(commercialData$CURRENT_LAND_VALUE, breaks=c(5, 9.609,10.316,11.184,15.182), labels=c("xs","s","m","l"))
commercialData$STRUCTURE_VALUE = cut(commercialData$STRUCTURE_VALUE, breaks=c(7, 11.118,11.788,12.688,18.274), labels=c("xs","s","m","l"))


summary(commercialData[ ,c("CrowdednessRate", "DisabledRate", "HSCompletedRate")])
# Try CrowdednessRate breaks 0, 0.0099, 0.0183, 0.0306, 0.0827
# Try DisabledRate breaks 0.02, 0.146, 0.187, 0.221, 0.442
# Try HSCompletedRate breaks 0.5232, 0.6975, 0.7903, 0.8823, 0.9994
# Try IncomeRate breaks 14781, 20491, 27270, 35989, 78354
commercialData$CrowdednessRate = cut(commercialData$CrowdednessRate, breaks=c(-1, 0.009941, 0.018340, 0.030642, 0.082765), labels=c("xs","s","m","l"))
commercialData$DisabledRate = cut(commercialData$DisabledRate, breaks=c(-1, 0.14630, 0.18720, 0.22160, 0.44260), labels=c("xs","s","m","l"))
commercialData$HSCompletedRate = cut(commercialData$HSCompletedRate, breaks=c(-1, 0.6975, 0.7903, 0.8823, 0.9994), labels=c("xs","s","m","l"))


summary(commercialData[ ,c("BothParentsRate", "UnemploymentRate", "YoungUnemploymentRate", "MalePopRate")])
# Try BothParentsRate breaks 0, 0.1387, 0.2132, 0.3286, 0.9415
# Try UnemploymentRate breaks 0.018, 0.08106, 0.14996, 0.2300, 0.3673 
# Try YoungUnemploymentRate breaks 0,0.9758, 0.2379, 0.380, 0.934
# Try MalePopRate breaks 0.3954, 0.4693, 0.4883, 0.5146, 0.6280
commercialData$BothParentsRate = cut(commercialData$BothParentsRate, breaks=c(-1, 0.1387, 0.2132, 0.3286, 0.9415), labels=c("xs","s","m","l"))
commercialData$UnemploymentRate = cut(commercialData$UnemploymentRate, breaks=c(0, 0.08106, 0.14996, 0.23008, 0.36734), labels=c("xs","s","m","l"))
commercialData$YoungUnemploymentRate = cut(commercialData$YoungUnemploymentRate, breaks=c(-1,0.9758, 0.2379, 0.38020, 0.93478), labels=c("xs","s","m","l"))
commercialData$MalePopRate = cut(commercialData$MalePopRate, breaks=c(0, 0.4693, 0.4883, 0.5146, 0.6280), labels=c("xs","s","m","l"))

summary(commercialData[ ,c("PovertyRate", "NonWhiteRate", "ChildrenRate", "ElderlyRate")])
# Try PovertyRate breaks 0.0011, 0.2392, 0.3363, 0.4593, 0.670
# Try NonWhiteRate breaks 0.07, 0.29809, 0.582, 0.74088, 0.9780
# Try ChildrenRate breaks 0,0.0414, 0.0693, 0.091, 0.159
# Try ElderlyRate breaks 0, 0.066, 0.0828, 0.102, 0.327
commercialData$PovertyRate = cut(commercialData$PovertyRate, breaks=c(0, 0.2392, 0.3363, 0.4593, 0.670), labels=c("xs","s","m","l"))
commercialData$NonWhiteRate = cut(commercialData$NonWhiteRate, breaks=c(0, 0.29809, 0.582, 0.74088, 0.9780), labels=c("xs","s","m","l"))
commercialData$ChildrenRate = cut(commercialData$ChildrenRate, breaks=c(-1,0.0414, 0.0693, 0.091, 0.159), labels=c("xs","s","m","l"))
commercialData$ElderlyRate = cut(commercialData$ElderlyRate, breaks=c(-1, 0.066, 0.0828, 0.102, 0.327), labels=c("xs","s","m","l"))

summary(commercialData[ ,c("VacancyRate", "OwnerOccupiedRate" , "MedianIncome")])
commercialData$VacancyRate = cut(commercialData$VacancyRate, breaks=c(0, 0.0916244, 0.1299818, 0.1770376, 0.3163713), labels=c("xs","s","m","l"))
commercialData$OwnerOccupiedRate = cut(commercialData$OwnerOccupiedRate, breaks=c(0, 0.1700807, 0.2772096, 0.4192222, 0.8236079), labels=c("xs","s","m","l"))
commercialData$MedianIncome = cut(commercialData$MedianIncome, breaks=c(14781, 20504, 27295, 36025, 78354), labels=c("xs","s","m","l"))


#install.packages("e1071")
library('e1071')
trainNum = sample(1:nrow(commercialData), 0.667*nrow(commercialData))
commercialData.train = commercialData[trainNum, ]
commercialData.test = commercialData[-trainNum, ]

# Build a model with Naive Bayes Classifer for Discrete Predictors

model = naiveBayes(commercialData.train[,-26], commercialData.train$FIRE)

# Predict the outcome
pred = predict(model, commercialData.test)
actual = commercialData.test$FIRE

# Display confusion matrix and accurary
cm <- (table(pred, actual))
cm

sum(diag(cm))/sum(cm)

# Due to the imbalanced FIRE feature, it is important to look at the performance in the 'YES' FIRE class.
# False Negative Rate (Yes FIRE predicted No FIRE) = 63%%
cm[1,2]/(cm[1,2] + cm[2,2])

# False Positive Rate (No FIRE predicted Yes FIRE) = 11%%
cm[2,1]/(cm[2,1] + cm[1,1])


###################Residential###################

data <- read.csv(file.choose(), header = TRUE, sep = ",")
head(data)

residentialData <- data
head(residentialData)


# Convert the integers to factors for features 3 to 6 (STORIES, BEDS, KITCHENS, BATHS).
# Note: features 10 to 15 (OVER_COND, BLDGSTYLE, EXT_WALL HEAT_TYPE, AIR_COND, FUEL_TYPE) are already factors.
for (i in 4:7) {
  residentialData[ , i] = as.factor(residentialData[ , i])
}


# Check that one of the features in the loop is indeed a factor now
class(residentialData$BEDS)


#========Discretization using summary() ==========#
# The following groupings within the features closely utilized the data point quantile distribution of the full dataset

# First check the summary for of the features.
summary(residentialData[ ,c("FIRST_FLOOR_AREA", "SECOND_FLOOR_AREA", "RESFLRAREA", "BUILD_AGE")])
# Try FIRST_FLOOR_AREA breaks 0, 709, 853, 1054, 4610
# Try SECOND_FLOOR_AREA breaks 0, 587, 838, 4070
# Try RESFLRAREA breaks 350, 1243, 1490, 1885, 10460
# Try BUILD_AGE breaks 0, 87, 95.51, 112, 220
residentialData$FIRST_FLOOR_AREA = cut(residentialData$FIRST_FLOOR_AREA, breaks=c(0, 709, 853, 1054, 4610), labels=c("xs","s","m","l"))
residentialData$SECOND_FLOOR_AREA = cut(residentialData$SECOND_FLOOR_AREA, breaks=c(-1, 640, 838, 4063), labels=c("s","m","l"))
residentialData$RESFLRAREA = cut(residentialData$RESFLRAREA, breaks=c(349, 1243, 1491, 1885, 10460), labels=c("xs","s","m","l"))
residentialData$BUILD_AGE = cut(residentialData$BUILD_AGE, breaks=c(-1, 87, 95.49, 112, 217), labels=c("xs","s","m","l"))


summary(residentialData[ ,c("CURRENT_LAND_VALUE", "STRUCTURE_VALUE")])
residentialData$CURRENT_LAND_VALUE = cut(residentialData$CURRENT_LAND_VALUE, breaks=c(4.6, 8.269, 8.683, 8.909, 11.918), labels=c("xs","s","m","l"))
residentialData$STRUCTURE_VALUE = cut(residentialData$STRUCTURE_VALUE, breaks=c(5.6, 10.472, 10.9, 12.290, 13.924), labels=c("xs","s","m","l"))


summary(residentialData[ ,c("CrowdednessRate", "DisabledRate", "HSCompletedRate")])
# Try CrowdednessRate breaks 0, 0.0081, 0.0185, 0.0304, 0.09
# Try DisabledRate breaks 0.6, 0.7874, 0.8151, 0.8569, 0.93
# Try HSCompletedRate breaks 0.5, 0.7221, 0.7919, 0.8822, 0.99
# Try IncomeRate breaks 14600, 22432, 30251, 35897, 83290
residentialData$CrowdednessRate = cut(residentialData$CrowdednessRate, breaks=c(-1, 0.008341, 0.018333, 0.030051, 0.09), labels=c("xs","s","m","l"))
residentialData$DisabledRate = cut(residentialData$DisabledRate, breaks=c(0, 0.14314, 0.18486, 0.21259, 0.39), labels=c("xs","s","m","l"))
residentialData$HSCompletedRate = cut(residentialData$HSCompletedRate, breaks=c(0, 0.7221, 0.7918, 0.8822, 0.99), labels=c("xs","s","m","l"))


summary(residentialData[ ,c("BothParentsRate", "UnemploymentRate", "YoungUnemploymentRate", "MalePopRate")])
# Try BothParentsRate breaks 0, 0.1617, 0.2600, 0.3610, 0.9999
# Try UnemploymentRate breaks 0.016, 0.08468, 0.15829, 0.2260, 0.4 
# Try YoungUnemploymentRate breaks 0, 0.1684, 0.3002, 0.4101, 0.99
# Try MalePopRate breaks 0.3, 0.4622, 0.4784, 0.4970, 0.7
residentialData$BothParentsRate = cut(residentialData$BothParentsRate, breaks=c(-1, 0.1636, 0.2625, 0.3638, 0.9999), labels=c("xs","s","m","l"))
residentialData$UnemploymentRate = cut(residentialData$UnemploymentRate, breaks=c(0, 0.08468, 0.15829, 0.22360, 0.4), labels=c("xs","s","m","l"))
residentialData$YoungUnemploymentRate = cut(residentialData$YoungUnemploymentRate, breaks=c(-1, 0.1684, 0.3002, 0.4101, 0.99), labels=c("xs","s","m","l"))
residentialData$MalePopRate = cut(residentialData$MalePopRate, breaks=c(0, 0.4623, 0.4784, 0.4971, 0.7), labels=c("xs","s","m","l"))

summary(residentialData[ ,c("PovertyRate", "NonWhiteRate", "ChildrenRate", "ElderlyRate")])
# Try PovertyRate breaks 0.05, 0.21784, 0.30046, 0.42992, 0.7
# Try NonWhiteRate breaks 0.06, 0.31058, 0.55900, 0.75219, 0.99
# Try ChildrenRate breaks 0.005,0.05773, 0.08914, 0.16
# Try ElderlyRate breaks 0.03, 0.07225, 0.08799, 0.4
residentialData$PovertyRate = cut(residentialData$PovertyRate, breaks=c(0, 0.21784, 0.30046, 0.42992, 0.7), labels=c("xs","s","m","l"))
residentialData$NonWhiteRate = cut(residentialData$NonWhiteRate, breaks=c(0, 0.31058, 0.55900, 0.75219, 0.99), labels=c("xs","s","m","l"))
residentialData$ChildrenRate = cut(residentialData$ChildrenRate, breaks=c(0, 0.05773, 0.073002, 0.08914, 0.16), labels=c("xs","s","m","l"))
residentialData$ElderlyRate = cut(residentialData$ElderlyRate, breaks=c(0, 0.07225, 0.08799, 0.10894, 0.4), labels=c("xs","s","m","l"))


summary(residentialData[ ,c("VacancyRate", "OwnerOccupiedRate" , "MedianIncome")])
residentialData$VacancyRate = cut(residentialData$VacancyRate, breaks=c(0, 0.081143, 0.121651, 0.153165, 0.324433), labels=c("xs","s","m","l"))
residentialData$OwnerOccupiedRate = cut(residentialData$OwnerOccupiedRate, breaks=c(0, 0.29001, 0.40444, 0.51964, 0.83293), labels=c("xs","s","m","l"))
residentialData$MedianIncome = cut(residentialData$MedianIncome, breaks=c(14600, 22493, 30273, 35919, 83283), labels=c("xs","s","m","l"))


#install.packages("e1071")
library('e1071')
#residentialData <- na.omit(residentialData)
trainNum = sample(1:nrow(residentialData), 0.667*nrow(residentialData))
residentialData.train = residentialData[trainNum, ]
residentialData.test = residentialData[-trainNum, ]

# Build a model with Naive Bayes Classifer for Discrete Predictors

model = naiveBayes(residentialData.train[,-36], residentialData.train$FIRE)

# Predict the outcome
pred = predict(model, residentialData.test)
actual = residentialData.test$FIRE

# Display confusion matrix and accurary
cm <- table(pred,actual)
cm
sum(diag(cm))/sum(cm)

# Due to the imbalanced FIRE feature, it is important to look at the performance in the 'YES' FIRE class.
# False Negative Rate (Yes FIRE predicted No FIRE) = 44%%
cm[1,2]/(cm[1,2] + cm[2,2])

# False Positive Rate (No FIRE predicted Yes FIRE) = 27%%
cm[2,1]/(cm[2,1] + cm[1,1])




#################################################################################################
#Model Comparison #################

CommercialConfusionMatrix <- list(cm.tree.commercial,cm.rf.commercial,cm.bag.commercial,cm.boost.commercial)
ResidentialConfusionMatrix <- list(cm.tree.residential,cm.rf.residential,cm.bag.residential,cm.boost.residential)

CommercialModels <- data.frame(tree = NA, RF = NA, Bag = NA, Boost = NA)
CommercialModels[1,] <- sapply(CommercialConfusionMatrix, accuracy)
CommercialModels[2,] <- sapply(CommercialConfusionMatrix, tpr)
CommercialModels[3,] <- sapply(CommercialConfusionMatrix, fpr)
CommercialModels[4,] <- sapply(CommercialConfusionMatrix, fnr)
CommercialModels[5,] <- sapply(CommercialConfusionMatrix, predper)
CommercialModels <- cbind(c("Accuracy","TPR","FPR","FNR","Predicted_Positive"),CommercialModels)

ResidentialModels <- data.frame(tree = NA, RF = NA, Bag = NA, Boost = NA)
ResidentialModels[1,] <- sapply(ResidentialConfusionMatrix, accuracy)
ResidentialModels[2,] <- sapply(ResidentialConfusionMatrix, tpr)
ResidentialModels[3,] <- sapply(ResidentialConfusionMatrix, fpr)
ResidentialModels[4,] <- sapply(ResidentialConfusionMatrix, fnr)
ResidentialModels[5,] <- sapply(ResidentialConfusionMatrix, predper)
ResidentialModels <- cbind(c("Accuracy","TPR","FPR","FNR","Predicted_Positive"),ResidentialModels)


#NEW TRESHOLD
CommercialConfusionMatrix2 <- list(cm.rf.commercial2,cm.bag.commercial2,cm.boost.commercial2)
ResidentialConfusionMatrix2 <- list(cm.rf.residential2,cm.bag.residential2,cm.boost.residential2)

CommercialModels2 <- data.frame(RF = NA, Bag = NA, Boost = NA)
CommercialModels2[1,] <- sapply(CommercialConfusionMatrix2, accuracy)
CommercialModels2[2,] <- sapply(CommercialConfusionMatrix2, tpr)
CommercialModels2[3,] <- sapply(CommercialConfusionMatrix2, fpr)
CommercialModels2[4,] <- sapply(CommercialConfusionMatrix2, fnr)
CommercialModels2[5,] <- sapply(CommercialConfusionMatrix2, predper)
CommercialModels2 <- cbind(c("Accuracy","TPR","FPR","FNR","Predicted_Positive"),CommercialModels2)

ResidentialModels2 <- data.frame(RF = NA, Bag = NA, Boost = NA)
ResidentialModels2[1,] <- sapply(ResidentialConfusionMatrix2, accuracy)
ResidentialModels2[2,] <- sapply(ResidentialConfusionMatrix2, tpr)
ResidentialModels2[3,] <- sapply(ResidentialConfusionMatrix2, fpr)
ResidentialModels2[4,] <- sapply(ResidentialConfusionMatrix2, fnr)
ResidentialModels2[5,] <- sapply(ResidentialConfusionMatrix2, predper)
ResidentialModels2 <- cbind(c("Accuracy","TPR","FPR","FNR","Predicted_Positive"),ResidentialModels2)



################################################################################################
#Probability Prediction
#########################
#Commercial
#########################
commercial.probabilities <- predict(boosting.commercial ,newdata = commercialDF , n.trees = 5000, type = "response")
commercialoutput <- data.frame(SBL = commercial$SBL, PROB = commercial.probabilities)

commercialoutput$RiskLevel <- ""
commercialoutput$RiskLevel[commercialoutput$PROB>=0.6] <- "High"
commercialoutput$RiskLevel[commercialoutput$PROB<0.6 & commerciailoutput$PROB>=0.1] <- "Mid"
commercialoutput$RiskLevel[commercialoutput$PROB<0.1] <- "Low"
commercialoutput <- unique(commercialoutput)

#########################
#Residential
#########################
residential.probabilities <- predict(boosting.residential ,newdata = residentialDF, n.trees = 5000, type = "response")
residentialoutput <- data.frame(SBL = residential$SBL, PROB = residential.probabilities)

residentialoutput$RiskLevel <- ""
residentialoutput$RiskLevel[residentialoutput$PROB>=0.11] <- "High"
residentialoutput$RiskLevel[residentialoutput$PROB<0.11 & residentialoutput$PROB>=0.05] <- "Mid"
residentialoutput$RiskLevel[residentialoutput$PROB<0.05] <- "Low"
residentialoutput <- unique(residentialoutput)

#Save CSV
write.csv(commercialoutput, "CommercialDataOutput.csv")
write.csv(residentialoutput, "ResidentialDataOutput.csv")