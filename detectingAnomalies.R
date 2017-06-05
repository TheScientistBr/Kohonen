# install libraries if its not in your R

require(kohonen)
library(readr)

normal <- data.frame(trafic = rnorm(1000,25,3), 
                     prot = rep(c(80,443,21,22,23),c(100,100,100,100,100)),
                     user = rep(c(1,2,3,4,5),c(200,200,200,200,200)))
normal3 <- data.frame(trafic = rnorm(1000,20,5), 
                     prot = rep(c(80,443,21,22,23),c(100,100,100,100,100)),
                     user = rep(c(1,2,3,4,5),c(200,200,200,200,200)))

data_train_matrix <- as.matrix(scale(normal))
data_train_matrix3 <- as.matrix(scale(normal2))


som_grid <- somgrid(xdim = 20, ydim=20, topo="hexagonal")

som_model <- som(data_train_matrix, 
                 grid=som_grid, rlen=1000, 
                 alpha=c(0.05,0.01), keep.data = TRUE)
som_model3 <- som(data_train_matrix3, 
                 grid=som_grid, rlen=1000, 
                 alpha=c(0.05,0.01), keep.data = TRUE)

plot(som_model2, type="changes")
plot(som_model2, type="count")
plot(som_model2, type="dist.neighbours")
plot(som_model2, type="codes")
plot(som_model2, type = "property", property = som_model$codes[[1]][,1], 
     main=names(som_model$data)[4])





var <- 1 #define the variable to plot 
var_unscaled <- aggregate(as.numeric(normal[,var]), 
                          by=list(som_model$unit.classif), 
                          FUN=mean, simplify=TRUE)[,2] 

plot(som_model, type = "property", property=var_unscaled, 
     main=names(normal)[var])

## Com ruido ou anomalia
normal[125:130,]$trafic <- 650 
data_train_matrix <- as.matrix(scale(normal))
som_grid <- somgrid(xdim = 20, ydim=20, topo="hexagonal")

som_model <- som(data_train_matrix, 
                 grid=som_grid, rlen=1000, 
                 alpha=c(0.05,0.01), keep.data = TRUE)
par(mfrow=c(1,3))
plot(som_model, type="changes")
plot(som_model, type="count")
plot(som_model3, type="dist.neighbours")
plot(som_model, type="codes")
plot(som_model, type = "property", property = som_model$codes[[1]][,1], 
     main=names(som_model$data)[4])

var <- 1 #define the variable to plot 
var_unscaled <- aggregate(as.numeric(normal[,var]), 
                          by=list(som_model$unit.classif), 
                          FUN=mean, simplify=TRUE)[,2] 

plot(som_model, type = "property", property=var_unscaled, 
     main=names(normal)[var])

