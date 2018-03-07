######################################################
### Fit the classification model with testing data ###
######################################################

### Author: Yuting Ma
### Project 3
### ADS Spring 2016

test <- function(fit_train, dat_test){
  
  ### Fit the classfication model with testing data
  
  ### Input: 
  ###  - the fitted classification model using training data
  ###  -  processed features from testing images 
  ### Output: training model specification
  
  ### load libraries
  
  # This is our baseline, which is sift features + gbm
  # It takes a trained gbm and outputs predictions
  
  # Our advanced model is in Python (using tensorflow), check out the .ipynb file
  # The notebook is straightforward to run and output predictions
  
  library("gbm")
  
  pred <- predict(fit_train$fit, newdata = dat_test, 
                  n.trees = fit_train$iter, type = "response")
  
  return(as.numeric(pred > 0.5))
}

