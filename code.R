library(superheat)
library(tidyverse)
library(caret)
library(Metrics)
library(h2o)
library(plotly)


traindata <- read.csv(file = 'trainingData.csv')
traindata$FLOOR <- factor(traindata$FLOOR)
traindata$BUILDINGID <- factor(traindata$BUILDINGID)


traindata_loc <- traindata[,1:520]



traindata_loc[traindata_loc == 100] = -105

###pca
traindata_loc <- as.data.frame(t(scale(t(traindata_loc), center = T, scale = T)))
traindata_loc[is.na(traindata_loc)] = 0

pcatest <- prcomp(traindata_loc, scale. = F)


plot(pcatest$sdev/sum(pcatest$sdev))
sum(pcatest$sdev[1:100]/sum(pcatest$sdev))

plot(pcatest$x[,1], pcatest$x[,2], col = (traindata$BUILDINGID + 1))



set.seed(66)
trainIndex <- createDataPartition(traindata$BUILDINGID, p = 0.7, list = FALSE)

####pca'd model
pcad_data50 <- data.frame(pcatest$x[,1:100])
pcad_d50_train <- pcad_data50[trainIndex, ]
pcad_d50_test <- pcad_data50[-trainIndex, ]

traindata_train <- traindata[trainIndex, ]
traindata_test <- traindata[-trainIndex, ]

####old models####

pcad_traindatasets <- list()
pcad_testdatasets <- list()
pcad_valdatasets <- list()
pcad_totaltraindatasets <- list()


###
for (i in c('LONGITUDE', 'LATITUDE', 'FLOOR', 'BUILDINGID')) {
  pcad_traindatasets[[i]] <- cbind(pcad_d50_train, i = traindata_train[,i])
  pcad_testdatasets[[i]] <- cbind(pcad_d50_test, i = traindata_test[,i])
  pcad_valdatasets[[i]] <- cbind(valdata_pcd, i = valdata[,i])
  pcad_totaltraindatasets[[i]] <- cbind(pcad_data50, i = traindata[,i])
}

rfmodels_pca <- lapply(pcad_traindatasets, function(x) train(i ~ ., x, method = 'ranger'))
knnneighbors_pca <- lapply(pcad_totaltraindatasets, function(x) train(i ~ ., x, method = 'knn'))

preds_rf_pca <- lapply(rfmodels_pca, function(x) predict(x, pcad_d50_test))
preds_knn_pca <- lapply(knnneighbors_pca, function(x) predict(x, pcad_d50_test))
###

###metrics of the pca model -- rf
confusionMatrix(preds_rf_pca$FLOOR, traindata_test$FLOOR)

Metrics::rmse(preds_rf_pca$LONGITUDE, traindata_test$LONGITUDE)
plot(preds_rf_pca$LONGITUDE, traindata_test$LONGITUDE)

Metrics::rmse(preds_rf_pca$LATITUDE, traindata_test$LATITUDE)
plot(preds_rf_pca$LATITUDE, traindata_test$LATITUDE)



###metrics of the pca model -- knn
confusionMatrix(preds_knn_pca$FLOOR, traindata_test$FLOOR)

Metrics::rmse(preds_knn_pca$LONGITUDE, traindata_test$LONGITUDE)
plot(preds_knn_pca$LONGITUDE, traindata_test$LONGITUDE)

Metrics::rmse(preds_knn_pca$LATITUDE, traindata_test$LATITUDE)
plot(preds_knn_pca$LATITUDE, traindata_test$LATITUDE)


save (rfmodels_pca, knnneighbors_pca, file = 'rf_knn_pca_models.RData')




####running the models with h2o####


h2o.init()

h2o_traindatasets <- list()
h2o_testdatasets <- list()
h2o_valdatasets <- list()
h2o_totaltraindataset <- list()

#for (i in c('LONGITUDE', 'LATITUDE', 'FLOOR')) {
#  h2o_traindatasets[[i]] <- as.h2o(cbind(pcad_d50_train, i = traindata_train[,i]))
#  h2o_testdatasets[[i]] <- as.h2o(cbind(pcad_d50_test, i = traindata_test[,i]))
#  h2o_valdatasets[[i]] <- as.h2o(cbind(valdata_pcd, i = valdata[,i]))
#}

h2o_traindatasets <- lapply(pcad_traindatasets, function(x) as.h2o(x))
h2o_testdatasets <- lapply(pcad_testdatasets, function(x) as.h2o(x))
h2o_valdatasets <- lapply(pcad_valdatasets, function(x) as.h2o(x))
h2o_totaltraindatasets <- lapply(pcad_totaltraindatasets, function(x) as.h2o(x))



h2o_rfs_val <- map2 (h2o_totaltraindatasets, h2o_valdatasets, 
                   function(x,y) h2o.randomForest(y = 'i', training_frame = x, validation_frame = y, ntrees = 200, max_depth = 50))

h2o_gbm_val <- map2 (h2o_totaltraindatasets, h2o_valdatasets, 
                    function(x,y) h2o.gbm (y = 'i', training_frame = x, validation_frame = y, max_depth = 30))



h2o_val_predicts <- map2(h2o_gbm_val, h2o_valdatasets, function(x,y) h2o.predict(x,y))

Metrics::rmse(valdata$LONGITUDE, as.vector(h2o_val_predicts$LONGITUDE))
confusionMatrix(factor(valdata$FLOOR), h2o_val_predicts$FLOOR)

plot(valdata$LATITUDE, as.vector(h2o_val_predicts$LATITUDE), col = valdata$FLOOR)


plot(valdata$LATITUDE, as.vector(h2o_val_predicts$LATITUDE))
plot(valdata$FLOOR, as.vector(h2o_val_predicts$FLOOR))


