library(superheat)
library(tidyverse)
library(caret)
library(Metrics)
library(h2o)
library(plotly)


traindata <- read.csv(file = 'trainingData.csv')


traindata_loc <- traindata[,1:520]



traindata_loc[traindata_loc == 100] = -105

#sds <- apply(traindata_loc, 2, sd)
#topsdhotspots <- order(sds, decreasing = TRUE)[1:30]

#sdsnonzeros <- sds != 0
#traindata_loc <- traindata_loc[,sdsnonzeros]

###pca
traindata_loc <- as.data.frame(t(scale(t(traindata_loc), center = T, scale = T)))
traindata_loc[is.na(traindata_loc)] = 0

pcatest <- prcomp(traindata_loc, scale. = F)


plot(pcatest$sdev/sum(pcatest$sdev))
sum(pcatest$sdev[1:50]/sum(pcatest$sdev))

plot(pcatest$x[,1], pcatest$x[,2])



set.seed(66)
trainIndex <- createDataPartition(traindata$BUILDINGID, p = 0.7, list = FALSE)

####pca'd model
pcad_data50 <- data.frame(pcatest$x[,1:50])
pcad_d50_train <- pcad_data50[trainIndex, ]
pcad_d50_test <- pcad_data50[-trainIndex, ]

traindata$FLOOR <- factor(traindata$FLOOR)
traindata_train <- traindata[trainIndex, ]
traindata_test <- traindata[-trainIndex, ]



pcad_traindatasets <- list()
pcad_testdatasets <- list()

load('rf_knn_pca_models.RData')
###
for (i in c('LONGITUDE', 'LATITUDE', 'FLOOR')) {
  pcad_traindatasets[[i]] <- cbind(pcad_d50_train, i = traindata_train[,i])
  pcad_testdatasets[[i]] <- cbind(pcad_d50_test, i = traindata_test[,i])
}

rfmodels_pca <- lapply(pcad_traindatasets, function(x) train(i ~ ., x, method = 'ranger'))
knnneighbors_pca <- lapply(pcad_traindatasets, function(x) train(i ~ ., x, method = 'knn'))

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




####running the models with h2o
h2o.init()

h2o_traindatasets <- list()
h2o_testdatasets <- list()
h2o_valdatasets <- list()

for (i in c('LONGITUDE', 'LATITUDE', 'FLOOR')) {
  h2o_traindatasets[[i]] <- as.h2o(cbind(pcad_d50_train, i = traindata_train[,i]))
  h2o_testdatasets[[i]] <- as.h2o(cbind(pcad_d50_test, i = traindata_test[,i]))
  h2o_valdatasets[[i]] <- as.h2o(cbind(valdata_pcd, i = valdata[,i]))
}


#h2orf_lat <- h2o.randomForest(y = 'i', training_frame = h2o_traindatasets$LONGITUDE, ntrees = 500)



h2o_rfs_val <- map2 (h2o_traindatasets, h2o_valdatasets, 
                   function(x,y) h2o.randomForest(y = 'i', training_frame = x, validation_frame = y, ntrees = 500))


h2o_val_predicts <- map2(h2o_rfs_val, h2o_valdatasets, function(x,y) h2o.predict(x,y))

Metrics::rmse(as.vector(h2o_valdatasets$LATITUDE$LATITUDE), as.vector(h2o_val_predicts$LATITUDE))
Metrics::rmse(valdata$LONGITUDE, as.vector(h2o_val_predicts$LONGITUDE))
confusionMatrix(valdata$FLOOR, h2o_val_predicts$FLOOR)

plot(valdata$LONGITUDE, as.vector(h2o_val_predicts$LONGITUDE))
plot(valdata$LATITUDE, as.vector(h2o_val_predicts$LATITUDE))
plot(valdata$FLOOR, as.vector(h2o_val_predicts$FLOOR))


