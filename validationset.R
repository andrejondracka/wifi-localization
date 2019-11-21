valdata <- read.csv('validationData.csv')

###take the data and transform it by removing the useless routers from train set and plus transform using the same components
valdata$FLOOR <- factor(valdata$FLOOR)
valdata$BUILDINGID <- factor(valdata$BUILDINGID)


valdata_loc <- valdata[,1:520]
valdata_loc[valdata_loc == 100] = -1000
#valsds <- apply(valdata_loc, 2, sd)

valdata_loc_cs <- t(scale(t(valdata_loc)))
valdata_loc_cs[is.nan(valdata_loc_cs)] = 0

valdata_pcd <-valdata_loc_cs %*% pcatest$rotation
valdata_pcd <- as.data.frame(valdata_pcd[,1:50])

###train dataset model prediction

preds_rf_pca_onval <- lapply(rfmodels_pca, function(x) predict(x, valdata_pcd))
preds_knn_pca_onval <- lapply(knnneighbors_pca, function(x) predict(x, valdata_pcd))


###metrics of the pca model -- rf
confusionMatrix(preds_rf_pca_onval$FLOOR, valdata$FLOOR)

Metrics::rmse(preds_rf_pca_onval$LONGITUDE, valdata$LONGITUDE)
plot(preds_rf_pca_onval$LONGITUDE, valdata$LONGITUDE)

Metrics::rmse(preds_rf_pca_onval$LATITUDE, valdata$LATITUDE)
plot(preds_rf_pca_onval$LATITUDE, valdata$LATITUDE)

###metrics of the pca model -- knn
confusionMatrix(preds_knn_pca_onval$FLOOR, valdata$FLOOR)

Metrics::rmse(preds_knn_pca_onval$LONGITUDE, valdata$LONGITUDE)
plot(preds_knn_pca_onval$LONGITUDE, valdata$LONGITUDE)

Metrics::rmse(preds_knn_pca_onval$LATITUDE, valdata$LATITUDE)
plot(preds_knn_pca_onval$LATITUDE, valdata$LATITUDE)

