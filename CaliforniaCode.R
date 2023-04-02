#California data classification model

suppressMessages(library(naivebayes))
suppressMessages(library(dplyr))
suppressMessages(library(ggplot2))
suppressMessages(library(psych))

load("/Users/snehashah/Downloads/predictors (1).Rdata")

data <- as.data.frame(Twb.prof) # vertical temperature profiles
ncol(data) # temperature is measured at 31 locations from the ground up through the upper atmosphere

###############################################################
# Plotting US Station Locations, Extracting California Observations
###############################################################

suppressMessages(library(purrr))
lon.new <- lon-360

plot(lon.new,lat,pch=19, main = "Stations Locations in the United States", xlab = "Longitude", ylab = "Latitude")
abline(v = -125, col = c("red"), lwd = 2)
abline(v = -113, col = c("red"), lwd = 2)
abline(h = 43, col = c("red"), lwd = 2)
abline(h = 32, col = c("red"), lwd = 2)
#abline(v = -107, col = c("red"), lwd = 2)
#abline(v = -93, col = c("red"), lwd = 2)
#abline(h = 36.5, col = c("red"), lwd = 2)
#abline(h = 25.2, col = c("red"), lwd = 2)
maps::map(database = "world",add=T)
maps::map("state",add=T)

################################################################################
# Identify stations in California
################################################################################

acali <- c()
acali_lat <- c()
acali_lon <- c()
acali_elev <- c()
for(i in 1:length(stations)){
  if (lat[i] > 32 & lat[i] < 43 & lon.new[i] > -124.5 & lon.new[i] < -114) {
    acali <- c(acali, i)
    acali_lat <- c(acali_lat, lat[i])
    acali_lon <- c(acali_lon, lon.new[i])
    acali_elev <- c(acali_elev, elev[i])
  }
}

plot(acali_lon, acali_lat, pch=19, main = "Station Locations Within LAT/LONG boundaries", xlab = "Longitude", ylab = "Latitude")

# label stations with indices on the map
for (i in 1:length(acali)) {
  text(acali_lon[i], acali_lat[i], labels=i, pos=2, offset=0, cex=0.9, font=1)
}
maps::map(database = "world",add=T)
maps::map("state",add=T)


###############################################################################################
# Filter out non-California stations by finding the indexes in acali vector from previous map plot
###############################################################################################

out_of_cali <- c(6, 7, 10, 13, 14, 15, 17, 27, 38, 40) # manually identify stations located outside of Cali state boundaries

# exclude non-Cali stations from Cali data
cali <- acali[-out_of_cali]
cali_lat <- acali_lat[-out_of_cali]
cali_lon <- acali_lon[-out_of_cali]
cali_elev <- acali_elev[-out_of_cali]

plot(cali_lon, cali_lat, pch=19, main = "Cali Station Locations", sub = "Figure 1: Stations located in Cali, United States", cex.sub = 1.1, xlab = "Longitude", ylab = "Latitude", xlim = c(-124.5,-114))
maps::map(database = "world",add=T)
maps::map("state",add=T)

cali_station.ind <- c()
cali_date.ind <- c()
cali_ptype <- c()
cali_Twb.prof.ind <- c()
for(i in 1:length(station.ind)){
  if (station.ind[i] %in% cali) {
    cali_station.ind <- c(cali_station.ind, station.ind[i])
    cali_date.ind <- c(cali_date.ind, date.ind[i])
    cali_ptype <- c(cali_ptype, ptype[i])
    cali_Twb.prof.ind <- c(cali_Twb.prof.ind, i) 
  }
}

cali_Twb.prof <- Twb.prof[cali_Twb.prof.ind, ]

step.size <- seq(0, 3000, by = 100)


# Extract month and year

year <- as.numeric(substr(dates, 1, 4))
unique(year) # years -- [1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013]

month <- as.numeric(substr(dates, 5, 6))
sort(unique(month)) # months -- [1  2  3  4  5  9 10 11 12]; excludes summer months, since the only type of precipitation that occurs during those months is rain

############################################################################################################################
############################################################################################################################
## CLASSIFICATION
############################################################################################################################
############################################################################################################################

caliAvgTemps <- c()
for (i in 1:nrow(cali_Twb.prof)){
  caliAvgTemps <- c(caliAvgTemps, mean(cali_Twb.prof[i,]))
}

cali_data <- data.frame(cali_ptype, cali_station.ind, cali_date.ind, caliAvgTemps)
colnames(cali_data) <- c("PType", "Station", "Date", "Temp")
dim(cali_data)

##################################################################
# Splitting Cali data into training, validation, and testing sets
##################################################################
cali_data <- na.omit(cali_data)
set.seed(1)
randomize <- sample(1:nrow(cali_data))
#training_index <- randomize[1:12519] # 60% #1:3130
#validation_index <- randomize[12519:16692] # 20% #3131:4173
#testing_index <- randomize[16692:20864] # 20% #4174:5216

#training_index <- randomize[1:3130] # 60% #1:3130
testing_index <- randomize[1:1043]
#validation_index <- randomize[3131:4173]# 20% #3131:4173
validation_index <- randomize[1044:2085]
#testing_index <- randomize[4174:5216]
training_index <- randomize[2086:5216]
train <- cali_data[training_index,]
validation <- cali_data[validation_index,]
test <- cali_data[testing_index,]

#######################################################
# Naive Bayes
#######################################################

trainModel <- naive_bayes(PType ~ ., data = train)
trainModel
plot(trainModel, arg.num = list(main = paste0("Gaussian Naive Bayes Classifier"), col = c("red", "green", "deepskyblue", "chocolate1"), lty = 1, lwd = 2))

#######################################################
# Naive Bayes Model Calibration: Brier Score
#######################################################

# Accuracy = 1 - Brier Score

pTrainNB <- predict(trainModel)
ft <- as.numeric(pTrainNB)
ot <- as.numeric(as.factor(train$PType))
trainBS_NB <- mean((ft - ot)^2)
trainBS_NB
trainA_NB <- 1 - trainBS_NB
trainA_NB

pValidationNB <- predict(trainModel, validation)
ft <- as.numeric(pValidationNB)
ot <- as.numeric(as.factor(validation$PType))
validationBS_NB <- mean((ft - ot)^2)
validationBS_NB
validationA_NB <- 1 - validationBS_NB
validationA_NB

pTestNB <- predict(trainModel, test)
ft <- as.numeric(pTestNB)
ot <- as.numeric(as.factor(test$PType))
testBS_NB <- mean((ft - ot)^2)
testBS_NB
testA_NB <- 1 - testBS_NB
testA_NB

#######################################################
# KNN
#######################################################

train <- cali_data[training_index,]
train$PType <- as.numeric(as.factor(train$PType))

validation <- cali_data[validation_index,]
validation$PType <- as.numeric(as.factor(validation$PType))

test <- cali_data[testing_index,]
test$PType <- as.numeric(as.factor(test$PType))


suppressMessages(library(class))

#train_predictions <- knn(train[,-dep_ndx], train[,-dep_ndx], train[,dep_ndx])
#cMTrain <- confusionMatrix(train_predictions, train$PType)
#train_accuracy <- mean(train_predictions == train[,dep_ndx])

#validation_predictions <- knn(train[,-dep_ndx], validation[,-dep_ndx], train[,dep_ndx])
#cMValidation <- confusionMatrix(validation_predictions, validation$PType)
#validation_accuracy <- mean(validation_predictions == validation[,dep_ndx])

#test_predictions <- knn(train[,-dep_ndx], test[,-dep_ndx], train[,dep_ndx])
#cMTest <- confusionMatrix(test_predictions, test$PType)
#test_accuracy <- mean(test_predictions == test[,dep_ndx])

dep_ndx <- which(colnames(cali_data) == "PType")

for (v in 1:ncol(cali_data)){
  if (v == dep_ndx) {next}
  s <- sd(train[,v])
  train[,v] <- train[,v] / s
  validation[,v] <- validation[,v] / s
  test[,v] <- test[,v] / s
}


train_accuracy <- validation_accuracy <- test_accuracy <- NULL
dep_ndx <- which(colnames(train) == 'PType')
k_vals <- seq(1, 50, by=4)

for (k in k_vals) {
  train_predictions <- knn(train[,-dep_ndx], train[,-dep_ndx], train[,dep_ndx], k=k)
  validation_predictions <- knn(train[,-dep_ndx], validation[,-dep_ndx], train[,dep_ndx], k=k)
  test_predictions <- knn(train[,-dep_ndx], test[,-dep_ndx], train[,dep_ndx], k=k)
  train_accuracy <- append(train_accuracy, mean(train$PType == train_predictions))
  validation_accuracy <- append(validation_accuracy, mean(validation$PType == validation_predictions))
  test_accuracy <- append(test_accuracy, mean(test$PType == test_predictions))
}

par(mfrow=c(1,3))
plot(k_vals, train_accuracy, main="Training", xlab = "k Values", ylab = "Accuracy", ylim = c(0.94, 1), col = c("red"), pch = 19)
plot(k_vals, validation_accuracy, main="Validation", xlab = "k Values", ylab = "Accuracy", ylim = c(0.94, 1), col = c("purple"), pch = 19)
plot(k_vals, test_accuracy, main="Testing", xlab = "k Values", ylab = "Accuracy", ylim = c(0.94, 1), col = c("deepskyblue"), pch = 19)


train_accuracy_KNN <- mean(train_accuracy)
validation_accuracy_KNN <- mean(validation_accuracy)
test_accuracy_KNN <- mean(test_accuracy)

train_accuracy_KNN
validation_accuracy_KNN
test_accuracy_KNN

#######################################################
# KNN Model Calibration: Brier Score
#######################################################

train_BS_KNN <- 1 - train_accuracy_KNN
validation_BS_KNN <- 1 - validation_accuracy_KNN
test_BS_KNN <- 1 - test_accuracy_KNN

train_BS_KNN
validation_BS_KNN
test_BS_KNN


#######################################################
# Decision Tree
#######################################################

suppressMessages(library(rpart))
suppressMessages(library(rpart.plot))

train <- cali_data[training_index,]
train$PType <- as.numeric(as.factor(train$PType))

validation <- cali_data[validation_index,]
validation$PType <- as.numeric(as.factor(validation$PType))

test <- cali_data[testing_index,]
test$PType <- as.numeric(as.factor(test$PType))

train$PType <- factor(train$PType)
dtModel <- rpart(PType ~ Temp, data = train)
print(dtModel)
par(cex = 1.0)
tree <- rpart.plot(dtModel)
tree <- rpart.plot(dtModel, box.col = c("white"))

train_predictions <- predict(dtModel, train, type="class")
cMTrain <- confusionMatrix(train_predictions, train$PType)
train_accuracy_DT <- mean(train_predictions == train[,dep_ndx])

validation_predictions <- predict(dtModel, validation, type="class")
cMValidation <- confusionMatrix(validation_predictions, validation$PType)
validation_accuracy_DT <- mean(validation_predictions == validation[,dep_ndx])

test_predictions <- predict(dtModel, test, type="class")
cMTest <- confusionMatrix(test_predictions, test$PType)
test_accuracy_DT <- mean(test_predictions == test[,dep_ndx])

train_accuracy_DT
validation_accuracy_DT
test_accuracy_DT

#######################################################
# Decision Tree Calibration: Brier Score
#######################################################

train_BS_DT <- 1 - train_accuracy_DT
validation_BS_DT <- 1 - validation_accuracy_DT
test_BS_DT <- 1 - test_accuracy_DT

train_BS_DT
validation_BS_DT
test_BS_DT

write.csv(cali_data, "calicode.csv")


