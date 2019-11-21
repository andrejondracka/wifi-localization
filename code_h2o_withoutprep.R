library(h2o)

traindata <- read.csv(file = 'trainingData.csv')
valdata <- read.csv('validationData.csv')


valdata$FLOOR <- factor(valdata$FLOOR)
valdata$BUILDINGID <- factor(valdata$BUILDINGID)
traindata$FLOOR <- factor(traindata$FLOOR)
traindata$BUILDINGID <- factor(traindata$BUILDINGID)


traindata_loc <- traindata[,1:520]
valdata_loc <- valdata[,1:520]

traindata_loc[traindata_loc == 100] = NA
valdata_loc[valdata_loc == 100] = NA


pcad_traindatasets <- list()
pcad_valdatasets <- list()

for (i in c('LONGITUDE', 'LATITUDE', 'FLOOR', 'BUILDINGID')) {
  pcad_traindatasets[[i]] <- cbind(traindata_loc, i = traindata[,i])
  pcad_valdatasets[[i]] <- cbind(valdata_loc, i = valdata[,i])
}

h2o.init()

h2o_traindatasets <- lapply(pcad_traindatasets, function(x) as.h2o(x))
h2o_valdatasets <- lapply(pcad_valdatasets, function(x) as.h2o(x))

h2o_rfs_noprep <- map2 (h2o_traindatasets, h2o_valdatasets, 
                     function(x,y) h2o.randomForest(y = 'i', training_frame = x, validation_frame = y, 
                                                    ntrees = 200, max_depth = 50, stopping_rounds = 2,
                                                    stopping_tolerance = 1e-2))

h2o_gbm_noprep <- map2 (h2o_traindatasets, h2o_valdatasets, 
                        function(x,y) h2o.gbm(y = 'i', training_frame = x, validation_frame = y))
                                                       

###predicting building first; splitting valdata frame according to predicted building

traindata_building <- cbind(traindata_loc, i=traindata$BUILDINGID)
h2o_traindata_building <- as.h2o(traindata_building)
h2o_valdata <- as.h2o(valdata_loc)

h2o_initbuild <- h2o.gbm(y = 'i', training_frame = h2o_traindata_building)

h2o_building_predicts <- h2o.predict(h2o_initbuild, h2o_valdata)
building_predicts <- as.data.frame(h2o_building_predicts)

valdata$building_predicted <- building_predicts$predict
valdata$building_predicted <- factor(valdata$building_predicted)


###running xgb models on training data split by building
traindata[traindata == 100] = NA
valdata[valdata == 100] = NA
predictornames <- colnames(traindata[,1:520])

train_bybuilding_list <- list()
val_bybuilding_list <- list()


for (i in unique(traindata$BUILDINGID)){
  train_bybuilding_list[[i]] <- subset(traindata, BUILDINGID == i)
}
  
for (i in unique(valdata$building_predicted)){
  val_bybuilding_list[[i]] <- subset(valdata, building_predicted == i)
}

h2o_train_bybuilding_list <- lapply(train_bybuilding_list, as.h2o)
h2o_val_bybuilding_list <- lapply(val_bybuilding_list, as.h2o)

h2o_gbm_lat <- map2 (h2o_train_bybuilding_list, h2o_val_bybuilding_list, 
                     function(xxx,yyy) h2o.gbm(x = predictornames, y = 'LATITUDE', training_frame = xxx, validation_frame = yyy))

tttt <- h2o.gbm(x = predictornames, y = 'LATITUDE', training_frame = h2o_train_bybuilding_list$`2`, validation_frame = h2o_val_bybuilding_list$`2`)
