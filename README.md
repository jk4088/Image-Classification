# Project 2: Predictive Modelling

*This is a group project to which I contributed for Spring 2018 STAT 4243 Applied Data Science at Columbia University.*

----


![](figs/dogscats_descrp.png)

Term: Spring 2018

+ Project title: Predictive Modelling - Dogs or Cats?
+ Team Number: Group 2
+ Team Members: Jerome Kafrouni, Ayano Kase, Joo Kim, Chunzi Wang, Chuyuan Zhou
+ Project summary: In this project, we built an image classification model that distinguishes between pictures of dogs and cats. The process consists of two parts: feature extraction and model training. We adopted SIFT, RGB, HOG, and HSV feature extraction methods to train on classification models GBM, Random Forest, XGBOOST, Logistics Regression, and SVM. In addition, we used Convolutional Neural Networks for feature extraction and classification. After model evaluation and comparison, we find that the optimal model is CNN. However, the trade-off between accuracy and time is an area for further improvement. 
+ Performance Analysis

![](figs/performance_results_updated.png)

#As for GBM, the we could see that the best result comes with SIFT feature extraction, and the best parameter is depth = 10. It comes with an accuracy of 72%, which is higher than with other feature extraction methods. The results for other feature extraction methods are around 63%, witch we acheived improvements using other models.


#### Contribution Statement

+ Jerome Kafrouni
  - Built and tested RGB and HSV in R and CNN in Python 
  - Served as guide to train and test all models 
  - Contributed to the presentation slide deck 
+ Ayano Kase
  - Built, trained, and tested SVM (linear and rbf) on all the features
  - Created README.md
  - Contributed to the presentation slide deck
+ Joo Kim
  - Built and tested HoG feature
  - Built, trained, and tested randomForest on all features 
  - Contributed to main README.md and updated sub-folder README.md's
  - Developed framework for and contributed to the presentation slide deck
  - Cleaned up GitHub repo 
+ Chunzi Wang
  - Built and tested SIFT feature
  - Built, trained, and tested logisticRegression on all features
  - Contributed README.md and presentation slide deck
+ Chuyuan Zhou
  - Built, trained, and tested GBM on all features 
  - Help organize and contributed to README.md
  - Contributed to the presentation slide deck

Following [suggestions](http://nicercode.github.io/blog/2013-04-05-projects/) by [RICH FITZJOHN](http://nicercode.github.io/about/#Team) (@richfitz). This folder is orgarnized as follows.

```
proj/
├── lib/
├── data/
├── doc/
├── figs/
└── output/
```

Please see each subfolder for a README file.
