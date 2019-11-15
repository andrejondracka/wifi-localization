library(plotly)

mergeddf <- traindata %>% group_by(LATITUDE, LONGITUDE, FLOOR) %>% sample_n(size = 1)
mergeddf$origin <- 'train'
mergeddf2 <- valdata %>% group_by(LATITUDE, LONGITUDE, FLOOR) %>% sample_n(size = 1)
mergeddf2$origin <- 'validation'

merged <- rbind(mergeddf, mergeddf2)


plot_ly(merged, x = ~LONGITUDE, y = ~LATITUDE, z = ~FLOOR, 
        color = ~origin, type = 'scatter3d')

plot_ly(merged, x = ~LONGITUDE, color = ~origin, type = 'box')

boxplot(mergeddf$LATITUDE, mergeddf2$LATITUDE, merged$LATITUDE)



mergedpredsval <- as.data.frame(cbind(LONGITUDE = preds_rf_pca_onval$LONGITUDE, 
                        LATITUDE = preds_rf_pca_onval$LATITUDE, 
                        FLOOR = preds_rf_pca_onval$FLOOR,
                        origin = 'prediction'))

mergedpredsval$LONGITUDE <- as.numeric(mergedpredsval$LONGITUDE)
mergedpredsval$LATITUDE <- as.numeric(mergedpredsval$LATITUDE)


  
plot_ly(mergedpredsval, x = ~LONGITUDE, y = ~LATITUDE, z = ~FLOOR, 
        type = 'scatter3d', size = 0.2)
  
merged <- merged[,c('LONGITUDE', 'LATITUDE', 'FLOOR', 'origin')]
merged$origin <- factor(merged$origin)
merged <- as.data.frame(merged)


merged2 <- rbind(merged, mergedpredsval)

plot_ly(merged2, x = ~LONGITUDE, y = ~LATITUDE, z = ~FLOOR, color = ~origin, 
        type = 'scatter3d', size = 0.2)
