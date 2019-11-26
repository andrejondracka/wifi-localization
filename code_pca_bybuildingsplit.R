library(tidyverse)
library(h2o)

traindata <- read.csv(file = 'trainingData.csv')
valdata <- read.csv('validationData.csv')


valdata$FLOOR <- factor(valdata$FLOOR)
valdata$BUILDINGID <- factor(valdata$BUILDINGID)
traindata$FLOOR <- factor(traindata$FLOOR)
traindata$BUILDINGID <- factor(traindata$BUILDINGID)


traindata_loc <- traindata[,1:520]
valdata_loc <- valdata[,1:520]

traindata_loc[traindata_loc == 100] = -105
valdata_loc[valdata_loc == 100] = -105



traindata_loc <- as.data.frame(t(scale(t(traindata_loc), center = T, scale = F)))
#traindata_loc <- traindata_loc + 105
traindata_loc[is.na(traindata_loc)] = 0

valdata_loc_cs <- t(scale(t(valdata_loc), center = T, scale = F))
#valdata_loc_cs <- valdata_loc + 105
valdata_loc_cs[is.nan(valdata_loc_cs)] = 0

###pca and selectinbg the first 50 (changeable by changing numpc value) components
pcatest <- prcomp(traindata_loc, center = F, scale. = F)
numpc <- 100 ###IMPORTANT PARMETER! DEFINES THE NUMBER OF PCs TO TAKE INTO ACCOUNT.

traindata_pcd <- data.frame(pcatest$x[,1:numpc])

valdata_pcd <-valdata_loc_cs %*% pcatest$rotation
valdata_pcd <- as.data.frame(valdata_pcd[,1:numpc])


###a model for building, predicting on val
h2o.init()

traindata_building <- cbind(traindata_pcd, i=traindata$BUILDINGID)
h2o_traindata_building <- as.h2o(traindata_building)
valdata_building <- cbind(valdata_pcd, i=valdata$BUILDINGID)
h2o_valdata_building <- as.h2o(valdata_building)

h2o_initbuild <- h2o.randomForest(y = 'i', training_frame = h2o_traindata_building, validation_frame = h2o_valdata_building,
                                  ntrees = 200, max_depth = 50)

h2o_building_predicts <- h2o.predict(h2o_initbuild, h2o_valdata_building)
building_predicts <- as.data.frame(h2o_building_predicts)

valdata$building_predicted <- building_predicts$predict
valdata$building_predicted <- factor(valdata$building_predicted)


###split datasets, run models for each
predictornames <- colnames(traindata_pcd[,1:numpc])

traindata_pcd2 <- cbind(traindata_pcd, traindata[,521:529])
valdata_pcd2 <- cbind(valdata_pcd, valdata[,521:530])

train_bybuilding_list <- list()
val_bybuilding_list <- list()

for (i in unique(traindata_pcd2$BUILDINGID)){
  train_bybuilding_list[[i]] <- subset(traindata_pcd2, BUILDINGID == i)
}

for (i in unique(valdata$building_predicted)){
  val_bybuilding_list[[i]] <- subset(valdata_pcd2, building_predicted == i)
}


h2o_train_bybuilding_list <- lapply(train_bybuilding_list, as.h2o)
h2o_val_bybuilding_list <- lapply(val_bybuilding_list, as.h2o)

#h2o_gbm_lat <- map2 (h2o_train_bybuilding_list, h2o_val_bybuilding_list, 
#                     function(xxx,yyy) h2o.gbm(x = predictornames, y = 'LATITUDE', training_frame = xxx, validation_frame = yyy))


h2o_gbm_lat_b2 <- h2o.gbm(x = predictornames, y = 'LATITUDE', training_frame = h2o_train_bybuilding_list$`2`, validation_frame = h2o_val_bybuilding_list$`2`)
h2o_gbm_lat_b0 <- h2o.gbm(x = predictornames, y = 'LATITUDE', training_frame = h2o_train_bybuilding_list$`0`, validation_frame = h2o_val_bybuilding_list$`0`)
h2o_gbm_lat_b1 <- h2o.gbm(x = predictornames, y = 'LATITUDE', training_frame = h2o_train_bybuilding_list$`1`, validation_frame = h2o_val_bybuilding_list$`1`)

h2o_gbm_fl_b2 <- h2o.gbm(x = predictornames, y = 'FLOOR', training_frame = h2o_train_bybuilding_list$`2`, validation_frame = h2o_val_bybuilding_list$`2`)
h2o_gbm_fl_b0 <- h2o.gbm(x = predictornames, y = 'FLOOR', training_frame = h2o_train_bybuilding_list$`0`, validation_frame = h2o_val_bybuilding_list$`0`)
h2o_gbm_fl_b1 <- h2o.gbm(x = predictornames, y = 'FLOOR', training_frame = h2o_train_bybuilding_list$`1`, validation_frame = h2o_val_bybuilding_list$`1`)

h2o_gbm_lon_b2 <- h2o.gbm(x = predictornames, y = 'LONGITUDE', training_frame = h2o_train_bybuilding_list$`2`, validation_frame = h2o_val_bybuilding_list$`2`)
h2o_gbm_lon_b0 <- h2o.gbm(x = predictornames, y = 'LONGITUDE', training_frame = h2o_train_bybuilding_list$`0`, validation_frame = h2o_val_bybuilding_list$`0`)
h2o_gbm_lon_b1 <- h2o.gbm(x = predictornames, y = 'LONGITUDE', training_frame = h2o_train_bybuilding_list$`1`, validation_frame = h2o_val_bybuilding_list$`1`)

###make predicts
h2o_val_bybuilding_list$`2`$lon_predict <- h2o.predict(h2o_gbm_lon_b2, h2o_val_bybuilding_list$`2`)
h2o_val_bybuilding_list$`0`$lon_predict <- h2o.predict(h2o_gbm_lon_b0, h2o_val_bybuilding_list$`0`)
h2o_val_bybuilding_list$`1`$lon_predict <- h2o.predict(h2o_gbm_lon_b1, h2o_val_bybuilding_list$`1`)

h2o_val_bybuilding_list$`2`$lat_predict <- h2o.predict(h2o_gbm_lat_b2, h2o_val_bybuilding_list$`2`)
h2o_val_bybuilding_list$`0`$lat_predict <- h2o.predict(h2o_gbm_lat_b0, h2o_val_bybuilding_list$`0`)
h2o_val_bybuilding_list$`1`$lat_predict <- h2o.predict(h2o_gbm_lat_b1, h2o_val_bybuilding_list$`1`)

h2o_val_bybuilding_list$`2`$floor_predict <- h2o.predict(h2o_gbm_fl_b2, h2o_val_bybuilding_list$`2`)$predict
h2o_val_bybuilding_list$`0`$floor_predict <- h2o.predict(h2o_gbm_fl_b0, h2o_val_bybuilding_list$`0`)$predict
h2o_val_bybuilding_list$`1`$floor_predict <- h2o.predict(h2o_gbm_fl_b1, h2o_val_bybuilding_list$`1`)$predict


####error analysis
valdata_finalpreds <- rbind(as.data.frame(h2o_val_bybuilding_list$`2`), as.data.frame(h2o_val_bybuilding_list$`1`), as.data.frame(h2o_val_bybuilding_list$`0`))

valdata_finalpreds$err_floor <- abs(as.numeric(valdata_finalpreds$FLOOR) - as.numeric(valdata_finalpreds$floor_predict))
valdata_finalpreds$err_building <- abs(as.numeric(levels(valdata_finalpreds$BUILDINGID))[valdata_finalpreds$BUILDINGID] 
                                       - as.numeric(levels(valdata_finalpreds$building_predicted))[valdata_finalpreds$building_predicted])
valdata_finalpreds$err_distance <- sqrt((valdata_finalpreds$LONGITUDE - valdata_finalpreds$lon_predict)^2 +
                                          (valdata_finalpreds$LATITUDE - valdata_finalpreds$lat_predict)^2)

valdata_finalpreds$err_total <- valdata_finalpreds$err_distance + 4*valdata_finalpreds$err_floor + 50*valdata_finalpreds$err_building

summary(valdata_finalpreds$err_total)








# h2o_val_b2 <- h2o.predict(h2o_gbm_b2, h2o_val_bybuilding_list$`2`[,1:50])
# 
# Metrics::rmse(valdata$LONGITUDE, as.vector(h2o_val_predicts$LONGITUDE))
# confusionMatrix(factor(valdata$FLOOR), h2o_val_predicts$FLOOR)
# 
# plot(val_bybuilding_list$`2`$LATITUDE, as.vector(h2o_val_predicts$LATITUDE))


