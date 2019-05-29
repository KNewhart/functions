NNopt <- function(all.data, predict.col.name, percent.train = 0.8, training.index=NULL) {
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
  
  ## Create training and test datasets
  if(is.null(training.index)) {
    set.seed(Sys.time())
    training.index <- sample(seq_len(nrow(all.data)), size=(percent.train*nrow(all.data)))
  }
  training.data <- all.data[training.index,]
  testing.data <- all.data[-training.index,]
  training.NN <- scaled.data[training.index,]
  testing.NN <- scaled.data[-training.index,]
  
  fmla <- as.formula(paste0(colnames(all.data)[predict.column],"~",
                            paste(colnames(all.data)[-predict.column], collapse= "+")))
  
  # detect cores with parallel() package
  nCores <- detectCores(logical = FALSE)
  # detect threads with parallel()
  nThreads<- detectCores(logical = TRUE)
  
  # Create doSNOW compute cluster
  cluster = makeCluster(nThreads, type = "SOCK")
  class(cluster);
  
  # register the cluster
  registerDoSNOW(cluster)
  
  
  # ## Find optimum number of hidden nodes
  # results <- foreach::foreach(i = 1:(ncol(all.data)-1), .combine = rbind) %dopar% {
  #   
  #   hidden.nodes <- i
  #   NN <- neuralnet::neuralnet(fmla, training.NN, hidden = hidden.nodes, linear.output = FALSE)
  #   
  #   ## Predict using NN
  #   predict.NN <-  neuralnet::compute(NN, testing.NN[,-predict.column])
  #   predict.NN <- (predict.NN$net.result * (max(all.data[,predict.column]) - min(all.data[,predict.column]))) + min(all.data[,predict.column])
  #   
  #   # Calculate Root Mean Square Error (RMSE)
  #   RMSE.NN <- (sum((testing.data[,predict.column] - predict.NN)^2) / nrow(testing.data)) ^ 0.5
  #   # Calculate R-sq
  #   source("GitHub\\functions\\r-sq.R")
  #   r2.NN <- r.sq(raw=testing.data[,predict.column], fit = predict.NN)
  #   
  #   data.frame("Hidden Nodes" = hidden.nodes,
  #              "RMSE" = RMSE.NN,
  #              "r2" = r2.NN)
  # }
  
  ## Find optimum number of variables
  x <- data.frame(combn(seq(2,ncol(all.data)),3))

  results <- foreach::foreach(i = 1:ncol(x), .combine = rbind) %dopar% {
    
    fmla <- as.formula(paste0(colnames(all.data)[predict.column],"~",
                              paste(colnames(all.data)[x[,i]], collapse= "+")))
    
    hidden.nodes <- i
    NN <- neuralnet::neuralnet(fmla, training.NN, hidden = hidden.nodes, linear.output = FALSE)
    
    ## Predict using NN
    predict.NN <-  tryCatch(neuralnet::compute(NN, testing.NN[,-predict.column]), error = function(e){})
    if(is.null(predict.NN)) next
    
    predict.NN <- (predict.NN$net.result * (max(all.data[,predict.column]) - min(all.data[,predict.column]))) + min(all.data[,predict.column])
    
    # Calculate Root Mean Square Error (RMSE)
    RMSE.NN <- (sum((testing.data[,predict.column] - predict.NN)^2) / nrow(testing.data)) ^ 0.5
    # Calculate R-sq
    source("GitHub\\functions\\r-sq.R")
    r2.NN <- r.sq(raw=testing.data[,predict.column], fit = predict.NN)
    
    data.frame("Hidden Nodes" = hidden.nodes,
               "Fmla" = fmla,
               "RMSE" = RMSE.NN,
               "r2" = r2.NN)
  }
  
  # stop cluster and remove clients
  stopCluster(cluster)
  
  # insert serial backend, otherwise error in repetetive tasks
  registerDoSEQ()
  
  # clean up a bit.
  invisible(gc); remove(nCores); remove(nThreads); remove(cluster); 
  
  ## Build best NN
  hidden.nodes <- min(results[,c("RMSE")])
  hidden.nodes <- which(results[,c("RMSE")] == hidden.nodes)
  hidden.nodes <- as.numeric(results[hidden.nodes,"Hidden.Nodes"])
  
  NN <- neuralnet(fmla, training.NN, hidden = hidden.nodes, linear.output = FALSE)
  
  ## Predict using NN
  predict.NN <-  neuralnet::compute(NN, testing.NN[,-predict.column])
  predict.NN <- (predict.NN$net.result * (max(all.data[,predict.column]) -
                                            min(all.data[,predict.column]))) + 
    min(all.data[,predict.column])
  
  # Calculate Root Mean Square Error (RMSE)
  RMSE.NN <- (sum((testing.data[,predict.column] - predict.NN)^2) / nrow(testing.data)) ^ 0.5
  
  
  return(list("NN" = NN,
              "Actual" = testing.data[,predict.column],
              "Predicted" = predict.NN,
              "RMSE" = RMSE.NN))
}