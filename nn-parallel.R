predict.col.name <- c("PAA...1.min..Sample")
percent.train = 0.8
all.data <- all.data[,which(colnames(all.data) != predict.col.name[2])]
iterations <- 10 # Number of times to run each model
x.combos = c(3,4,5,6,7,8,9) # Number of variables to be randomly selected to build models

NNopt <- function(all.data, predict.col.name, percent.train = 0.8, iterations = 1, x.combos=c(round(ncol(all.data)/2))) {
library(doSNOW)
library(parallel)
library(neuralnet)

## Prep data
predict.column <- which(colnames(all.data) == predict.col.name)
all.data <- na.omit(data.frame(all.data))

## Scale data using min-max method
max <- apply(all.data, 2, max)
min <- apply(all.data, 2, min)
scaled.data <- as.data.frame(scale(all.data, center = min, scale = max - min))

for(iteration in 1:iterations) {
  set.seed(Sys.time())
  training.index <- sample(seq_len(nrow(all.data)), size=(percent.train*nrow(all.data)))

  training.data <- all.data[training.index,]
  testing.data <- all.data[-training.index,]
  training.NN <- scaled.data[training.index,]
  testing.NN <- scaled.data[-training.index,]
  
  ## Find optimum number of variables
  n <- which(colnames(all.data) != predict.col.name)
  
  for(no.vars in x.combos) {
    
    x <- data.frame(combn(n,no.vars))
    x.index <- seq(1:ncol(x))
    
    # detect cores with parallel() package
    nCores <- detectCores(logical = FALSE)
    # detect threads with parallel()
    nThreads<- detectCores(logical = TRUE)
    
    # Create doSNOW compute cluster
    cluster = makeCluster(nThreads, type = "SOCK")
    class(cluster);
    
    # register the cluster
    registerDoSNOW(cluster)
    
    results <- foreach::foreach(i = x.index, .combine = rbind) %dopar% {
      
      fmla <- as.formula(paste0(colnames(all.data)[predict.column],"~",
                                paste(colnames(all.data)[x[,i]], collapse= "+")))
      nodes <- round(no.vars/2)
      NN <- neuralnet::neuralnet(fmla, training.NN, hidden = nodes, linear.output = FALSE)
      
      ## Predict using NN
      predict.NN <-  tryCatch(neuralnet::compute(NN, testing.NN[,-predict.column]), error = function(e){})
      if(is.null(predict.NN)){
        RMSE.NN <- NA
        r2.NN <- NA
      } else {
        predict.NN <- (predict.NN$net.result * (max(all.data[,predict.column]) - min(all.data[,predict.column]))) + min(all.data[,predict.column])
        # Calculate Root Mean Square Error (RMSE)
        RMSE.NN <- (sum((testing.data[,predict.column] - predict.NN)^2) / nrow(testing.data)) ^ 0.5
        # Calculate R-sq
        r2.NN <- r.sq(raw=testing.data[,predict.column], fit = predict.NN)
      }
      data.frame("Fmla" = as.character(fmla[3]),
                 "RMSE" = RMSE.NN,
                 "r2" = as.numeric(r2.NN),
                 "Nodes" = nodes)
    }
    
    # stop cluster and remove clients
    stopCluster(cluster)
    
    # insert serial backend, otherwise error in repetetive tasks
    registerDoSEQ()
    
    # clean up a bit.
    invisible(gc); remove(nCores); remove(nThreads); remove(cluster); 
    
    write.csv(results, paste0("results/",results[1,4],"nodes_",no.vars,"vars_",iteration,".csv"))
    
    
  }
}



file <- paste0("results/",list.files(path="results"))
for(i in 1:length(file)){
  raw <- read.csv(file[i], row.names=1, stringsAsFactors=FALSE)
  if(i ==1) data <- raw
  if(i != 1)data <- rbind(data,raw)
}



# detect cores with parallel() package
nCores <- detectCores(logical = FALSE)
# detect threads with parallel()
nThreads<- detectCores(logical = TRUE)

# Create doSNOW compute cluster
cluster = makeCluster(nThreads, type = "SOCK")
class(cluster);

# register the cluster
registerDoSNOW(cluster)

data.avg <- foreach::foreach(i = 1:length(unique(data$Fmla)), .combine = rbind) %dopar% {
  result <- mean(data$r2[which(data$Fmla == unique(data$Fmla)[i])])
  data.frame("Fmla" = unique(data$Fmla)[i],
             "R2" = result)
}

# stop cluster and remove clients
stopCluster(cluster)

# insert serial backend, otherwise error in repetetive tasks
registerDoSEQ()

# clean up a bit.
invisible(gc); remove(nCores); remove(nThreads); remove(cluster); 


# Rank r2 and RMSE to find best model

best.r2 <- which(data.avg[,2] == max(data.avg[,2]))
best.fmla <- unique(data$Fmla)[best.r2]

fmla <- best.fmla
nodes <- round(no.vars/2)
NN <- neuralnet::neuralnet(fmla, training.NN, hidden = nodes, linear.output = FALSE)

## Predict using NN
predict.NN <-  tryCatch(neuralnet::compute(NN, testing.NN[,-predict.column]), error = function(e){})
if(is.null(predict.NN)){
  RMSE.NN <- NA
  r2.NN <- NA
} else {
  predict.NN <- (predict.NN$net.result * (max(all.data[,predict.column]) - min(all.data[,predict.column]))) + min(all.data[,predict.column])
  # Calculate Root Mean Square Error (RMSE)
  RMSE.NN <- (sum((testing.data[,predict.column] - predict.NN)^2) / nrow(testing.data)) ^ 0.5
  # Calculate R-sq
  r2.NN <- r.sq(raw=testing.data[,predict.column], fit = predict.NN)
}

NNobj <- list(NN, predict.NN, RMSE.NN, r2.NN)
return(NNobj)
}