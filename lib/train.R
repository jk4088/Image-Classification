#########################################################
### Train a classification model with training images ###
#########################################################


train <- function(dat_train, label_train, model, par = NULL){
  
  ### Train a Gradient Boosting Model (GBM) using processed features from training images
  
  ### Input: 
  ###  -  processed features from images 
  ###  -  class labels for training images
  ### Output: training model specification
  
  if(model == "gbm"){
    return(train_gbm(dat_train, label_train, par))
  } else if (model == "randomForest"){
  return(train_randomForest(dat_train, label_train, par))
  } else if(model == "svm_rbf"){
    return(train_svm_rbf(dat_train, label_train, par))
  } else if(model == "svm_lin"){
    return(train_svm_lin(dat_train, label_train, par))
  } else if(model == "logisticRegression"){
    return(train_logisticRegression(dat_train, label_train, par))
  }
}

train_randomForest <- function(dat_train, label_train, par){
    library("randomForest")
    
    ### Train with randomForest
    if(is.null(par)){
        ntree <- 500
        mtry <- 50
    } else {
        ntree <- par$ntree
        mtry <- par$mtry
}
 
 fit_randomForest <- randomForest(x = dat_train,
 y = label_train,
 ntree = ntree,
 mtry = mtry)
 
 return(list(fit = fit_randomForest))
}

train_gbm <- function(dat_train, label_train, par){
  ### load libraries
  library("gbm")
  
  # dat_train is a dataframe but gbm needs a matrix:
  dat_train <- as.matrix(setNames(dat_train, NULL))
  ### Train with gradient boosting model
  if(is.null(par)){
    depth <- 3
  } else {
    depth <- par$depth
  }
  fit_gbm <- gbm.fit(x = dat_train, y = label_train,
                     n.trees = 2000,
                     distribution = "bernoulli",
                     interaction.depth = depth, 
                     bag.fraction = 0.5,
                     verbose = FALSE)
  best_iter <- gbm.perf(fit_gbm, method = "OOB", plot.it = FALSE)
  return(list(fit = fit_gbm, iter = best_iter))
}


train_svm_rbf = function(dat_train, label_train, par){
  
  ###  dat_train: processed features from images also contains label
  
  library(e1071)
  
  if(is.null(par)){
    Cost <- 5
    Gamma <- 100
  }
  else{
    Cost <- par$Cost
    Gamma <- par$Gamma
  }
  
  fit_svm_rbf <- svm(x = dat_train, y = label_train,  
                     kernel = "radial", scale = FALSE,
                     Cost = Cost, Gamma = Gamma)
  return(list(fit = fit_svm_rbf))
}
  

train_svm_lin = function(dat_train, label_train, par){
  
  ###  dat_train: processed features from images also contains label
  
  library(e1071)
  
  if(is.null(par)){
    Cost <- 1
  }
  else{
    Cost <- par
  }
  
  fit_svm_lin <- svm(x = dat_train, y = label_train,
                     method = "svmLinear", scale = FALSE,
                     Cost = Cost)
  return(list(fit = fit_svm_lin))
}

train_logisticRegression <- function(dat_train, label_train, par){
  fit_lr <- glm(label_train~., data = dat_train, family = "binomial")
  return(list(fit = fit_lr))
}
