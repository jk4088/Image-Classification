#############################################################
### Construct visual features for training/testing images ###
#############################################################

### Generic code for any feature, will call specific functions corresponding to the "method" chosen:
feature <- function(img_dir, indexes="all", method="row_mean", export=T){
  
  ### Construct process features for training/testing images
  ### Sample simple feature: Extract row average raw pixel values as features
  ### This function imports all the training files, and then passes them to one of the functions defined below
  ### Which will be responsible for building the actual features
  ### The choice of method to compute features is included in the keyword argument "method"
  
  ### When you're still working on the model (debugging, not actually training it), you can work on a subset of features
  ### in order to make your code run faster. To do so, pass a list of samples (numbers corresponding to training samples)
  ### as "indexes" argument.
  
  ### Input: a directory that contains images ready for processing
  ### Output: an .RData file contains processed features for the images
  
  library(EBImage)
  
  n_files <- length(list.files(img_dir))
  
  # Read images:
  images <- list()
  
  if (indexes == "all"){
    indexes <- seq(1, n_files)
  }
  
  if ((method != "sift") & (method != "sift_test")){
    for(i in indexes){
      img <- readImage(paste0(img_dir,  "pet", i, ".jpg"))
      images[[length(images) + 1]] <- img
    }
  }
  else{
    
  }
  
  
  # Call features generation function:
  if(method == "row_mean"){
    features <- row_mean_feature(images)
  } else if(method == "rgb"){
    features <- rgb_feature(images)
  } else if(method == "hsv"){
    features <- hsv_feature(images)
  } else if(method == "hog"){
    features <- hog_feature(images)
  } else if(method == "sift"){
    features <- sift_feature(images)
  } else if(method=="sift_test"){
    features <- sift_feature_test(images)
  }
  
  ### output constructed features
  # if(export){
  # save(features, file = paste0("../output/feature_", data_name, "_", set_name, ".RData"))
  # }
  return(data.frame(features))
}


### Functions for each way to construct features:
row_mean_feature <- function(images){
  # Basic function used for debugging:
  # This method turns images into grayscale, then computes mean intensity per row
  # To do so, we resize all images to 300 rows (and proportional number of columns)
  dat <- matrix(NA, length(images), 300)
  for(i in 1:length(images)){
    img <- images[[i]]
    gray_img <- channel(img,"gray")
    gray_img <- resize(gray_img, 300) # not specifying the last argument (y) lets keep proportions
    dat[i,] <- rowMeans(gray_img)
  }
  return(dat)
}

rgb_feature <- function(images){
  # we split the intensity range [0,1] into 12 bins, for each channel
  # therefore we get 1728 bins that are a partition of all possible colors
  # (since each color is a combination of red intensity, blue intensity, green intensity)
  
  # Number of bins per channel:
  nR <- 12
  nG <- 12
  nB <- 12 
  
  # Bins:
  rBin <- seq(0, 1, length.out=nR)
  gBin <- seq(0, 1, length.out=nG)
  bBin <- seq(0, 1, length.out=nB)
  
  dat <- matrix(NA, length(images), nR*nG*nB)
  
  ########extract RGB features############
  for (i in 1:length(images)){
    img <- images[[i]]
    img_as_rgb <-array(c(img,img,img),dim = c(nrow(img),ncol(img),3))
    # We compute a histogram of all possible RGB colors based on the bins defined above
    freq_rgb <- as.data.frame(table(factor(findInterval(img_as_rgb[,,1], rBin), levels=1:nR), 
                                    factor(findInterval(img_as_rgb[,,2], gBin), levels=1:nG),
                                    factor(findInterval(img_as_rgb[,,3], bBin), levels=1:nB)))
    dat[i,] <- as.numeric(freq_rgb$Freq)/(ncol(img)*nrow(img)) # normalization
  }
  return(dat)
}

hsv_feature <- function(images){
  # This function is similar to RGB feature except here we count frequencies based on HSV (Hue Saturation Value)
  # Instead of RGB channels
  
  # we split the intensity range [0,1] into 12 bins, for each channel
  # therefore we get 1728 bins that are a partition of all possible colors
  # (since each color is a combination of red intensity, blue intensity, green intensity)
  library(grDevices)
  # Number of bins per channel:
  nH <- 12
  nS <- 12
  nV <- 12 
  
  # Bins:
  hBin <- seq(0, 1, length.out=nH)
  sBin <- seq(0, 1, length.out=nS)
  vBin <- seq(0, 1, length.out=nV)
  
  dat <- matrix(NA, length(images), nH*nS*nV)
  
  ########extract RGB features############
  for (i in 1:length(images)){
    img <- images[[i]]
    img_as_rgb <-array(c(img,img,img),dim = c(nrow(img),ncol(img),3))
    dim(img_as_rgb) <- c(nrow(img_as_rgb)*ncol(img_as_rgb), 3) # resize for rgb2hsv
    img_hsv <- rgb2hsv(t(img_as_rgb))
    # We compute a histogram of all possible RGB colors based on the bins defined above
    freq_hsv <- as.data.frame(table(factor(findInterval(img_hsv[1,], hBin), levels=1:nH), 
                                    factor(findInterval(img_hsv[2,], sBin), levels=1:nS),
                                    factor(findInterval(img_hsv[3,], vBin), levels=1:nV)))
    dat[i,] <- as.numeric(freq_hsv$Freq)/(ncol(img)*nrow(img)) # normalization
  }
  return(dat)
}


hog_feature <- function(images){
  library("OpenImageR")
  
  n_files = length(images)
  cropped <- list()
  resized <- list()
  # Step 1: crop all images to square and centered, then resize to 64*64
  for (i in 1:n_files){
      cropped[[i]] <- if(ncol(images[[i]]) > nrow(images[[i]])){
          cropImage(images[[i]], nrow(images[[i]]) - 1, nrow(images[[i]]) - 1,
          type = "equal_spaced")
      } else {
          cropImage(images[[i]], ncol(images[[i]]) - 1, ncol(images[[i]]) - 1,
          type = "equal_spaced")
      }
      resized[[i]] <- resizeImage(cropped[[i]], 64, 64, method = "bilinear")
  }
  
  # Step 2: extract hog feature for each image (cell and orientation numbers may be changed with tuning)
  hog <- matrix(NA, nrow = n_files, ncol = 200)
  for (i in 1:n_files){
      hog[i, ] <- HOG(resized[[i]], cells = 5, orientations = 8)
  }
  
  return(hog)
}

sift_feature <- function(images) {
  # The sift features have already been computed
  # Download them from https://drive.google.com/a/columbia.edu/uc?id=128fqgPZa6I-ZlB_xqhFmO6KmdJyLN1nB&export=download.
  # And put them in your "output" folder
  
  # in a sift.Rmd, we compute clusters on sift features to be able to perform "Bag Of Features"
  # Here we load those clusters and then for each image assign all its keypoints to the clusters
  # To compute a bag of word representation of the image's sift values.
  
  # note that in sift.Rmd we also merged all the sift features files into one matrix that we saved in pets_sift.RData
  # this is too big to fit on the repo.
  library(dplyr)
  library(tidyr)
  
  clusters <- get(load("../output/clusters.RData"))
  pets_sift <- get(load("../output/pets_sift.RData"))
  
  # Step 5: data manipulation to put every image in a row and clusters in columns. (Bag of features)
  pets_sift_df <- as.data.frame(pets_sift)
  pets_sift_df$cluster <- clusters$cluster
  colnames(pets_sift_df)[129] <- "img"
  
  pets_sift_cluster <- pets_sift_df %>%
    group_by(img) %>%
    count(cluster) %>%
    spread(cluster,n)
  
  pets_sift_cluster <- pets_sift_cluster[-1,]
  
  pets_sift_cluster[is.na(pets_sift_cluster)] <- 0
  colnames(pets_sift_cluster) <-c("img",paste("cluster",1:100,sep="_"))
  
  return(pets_sift_cluster)
}

sift_feature_test <- function(images) {
  # The sift features have already been computed
  # Download them from https://drive.google.com/a/columbia.edu/uc?id=128fqgPZa6I-ZlB_xqhFmO6KmdJyLN1nB&export=download.
  # And put them in your "output" folder
  
  # in a sift.Rmd, we compute clusters on sift features to be able to perform "Bag Of Features"
  # Here we load those clusters and then for each image assign all its keypoints to the clusters
  # To compute a bag of word representation of the image's sift values.
  
  # note that in sift.Rmd we also merged all the sift features files into one matrix that we saved in pets_sift.RData
  # this is too big to fit on the repo.
  library(dplyr)
  library(tidyr)
  library(clue)
  
  clusters <- get(load("../output/clusters.RData"))
  # IMPORTANT NOTE :
  # This function needs a file test_sift that contains all the sift features aggregated in one Rdata file
  # This should be generated in the sift.Rmd file
  pets_sift <- get(load("../output/test_sift.RData"))

  # assign to existing clusters:
  pred_cl <- cl_predict(clusters, pets_sift)
  pets_sift_df <- as.data.frame(pets_sift)
  colnames(pets_sift_df)[129] <- "img"
  pets_sift_df$cluster <- pred_cl
  
  pets_sift_cluster <- pets_sift_df %>%
    group_by(img) %>%
    count(cluster) %>%
    spread(cluster,n)
  
  pets_sift_cluster <- pets_sift_cluster[-1,]
  
  pets_sift_cluster[is.na(pets_sift_cluster)] <- 0
  colnames(pets_sift_cluster) <-c("img",paste("cluster",1:100,sep="_"))
  
  return(pets_sift_cluster)
}
