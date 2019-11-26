library(plotly)

valdata_finalpreds$err_dist_round <- cut(valdata_finalpreds$err_distance, breaks = 11)



plot_ly(valdata_finalpreds, x = ~LONGITUDE, y = ~LATITUDE, z = ~FLOOR,  type = 'scatter3d', 
        color = ~err_dist_round, colors = 'Spectral', size = 0.2)
