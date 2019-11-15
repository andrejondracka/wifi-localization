####code - deprecated

kmeanstest <- kmeans(traindata_loc, centers = 10)
traindata_loc$k <- kmeanstest$cluster

head(traindatasetfloor[519:ncol(traindatasetfloor)])

traindata_loc_clumeans <- aggregate(traindata_loc, by=list(traindata_loc$k), FUN = mean)

superheat(traindata_loc_clumeans[,2:521], pretty.order.cols = T)

traindata_infos <-  apply(traindata_loc, 2, sd)

superheat(traindata_loc[1:200,1:520], membership.rows = traindata_loc$k[1:200])
superheat(traindata_loc[19000:19100,1:520], membership.rows = traindata_loc$k[19000:19100])




###run a random forest
trainIndex <- createDataPartition(traindata$BUILDINGID, p = 0.7, list = FALSE)
traindataset <- traindata[trainIndex, ]
testdataset <- traindata[-trainIndex, ]

testdataset_red <- testdataset[,topsdhotspots]

#predicting floor/building
#rfmodelo - original value
#rfmodelo2 - -105
#rfmodelo3 - -1000
traindatasetfloor <- traindataset[,c(1:520)]
traindatasetfloor$FLOOR <- factor(traindataset$FLOOR, ordered = TRUE)

traindatasetfloor_red <- cbind(traindatasetfloor[,topsdhotspots], FLOOR = traindatasetfloor$FLOOR)
traindatasetfloor_red[traindatasetfloor_red == -1000] = 100

rfmodelored_4 <- train(FLOOR ~ ., traindatasetfloor_red, method = 'ranger')

preds4 <- predict(rfmodelored_4, testdataset_red)
confusionMatrix(factor(testdataset$FLOOR, ordered = TRUE), preds4)

###loop for running models for longitude and latitude
traindatasetfloor <- traindatasetfloor[,topsdhotspots]

traindatasets <- list()
for (i in c('LONGITUDE', 'LATITUDE')) {
  traindatasets[[i]] <- cbind(traindatasetfloor, i = traindataset[,i])
}

rfmodels <- lapply(traindatasets, function(x) train(i ~ ., x, method = 'ranger'))

preds_latlon <- lapply(rfmodels, function(x) predict(x, testdataset_red))

Metrics::rmse(preds_latlon$LONGITUDE, testdataset$LONGITUDE)
Metrics::rmse(preds_latlon$LATITUDE, testdataset$LATITUDE)

plot(preds_latlon$LONGITUDE, testdataset$LONGITUDE)
plot(preds_latlon$LATITUDE, testdataset$LATITUDE)



