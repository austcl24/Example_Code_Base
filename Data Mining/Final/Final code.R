###################################
# DS 740 - Final: Chris Austin    #
###################################
require(class)
require(MVN)
voice = read.csv("voice.csv")

# Some notes out of the summary: 1. No missing data to deal with, so no cleanup
# necessary. 2. Dataset is evenly divided between male and female voice samples.
# No worries about undersampling/oversampling of genders as it relates to the
# predictive power of our eventual model.
summary(voice)

# Pull out the predictors and check to see if they are multivariate normal for
# male and female subsets of data.
male = voice[voice$label == "male",-21]
female = voice[voice$label == "female",-21]

#hztest_0 = mvn(male, mvnTest = "hz")
#hztest_1 = mvn(female, mvnTest = "hz")
# tests fail with errors that suggest multicollinearity. We'll need to look at cor
#   Lapack routine dgesv: system is exactly singular: U[20,20] = 0
#   Lapack routine dgesv: system is exactly singular: U[12,12] = 0

voiceCor = cor(voice[,-21])
(sum(abs(voiceCor) > .8 & voiceCor < 1)/2)
# For a 20x20 predictor matrix - less the 20 perfectly correlated values in the
# diagonal and one side mirrored across the diagonal - 13 of the 180 possible
# combinations have an absolute correlation greater than .8


# Using Principal Component Analysis to determine predictors contributing to variance.
pc.info = prcomp(voice[,-21], scale=TRUE, center = TRUE)
plot(pc.info)
summary(pc.info)
pc.info$sdev
vjs = pc.info$sdev^2
pve = vjs/sum(vjs)
cumsum(pve)

plot(cumsum(pve), type = "o", ylab="Cumulative PVE", xlab="Principal Component")

biplot(pc.info,scale=0, col = c("white", "red"), main = "Magnitude of Contribution to PC1 and PC2")
#pc.info$rotation[,1]  # loadings for first principal component
#pc.info$rotation[,2]  # loadings for second principal component
#pc1scores = pc.info$x[,1]  # first principal component score vector
#pc2scores = pc.info$x[,2]  # second principal component score vector

# Euclidian distances of magnitude for PC1 and PC2 for our 20 predictors. There
# looks to be a break in magnitude at the last three values: meanfun, modindx,
# and minfin, noting that those are the least weighted in PC1/PC2 space. Those
# variables should be left out of one option of our logistic regression
# analysis. Otherwise, most of the predictors supply similar contribution as
# noted in the biplot.
sort(sqrt(pc.info$rotation[,1]^2 + pc.info$rotation[,2]^2), decreasing = TRUE)

###################################################################
# Section 2 - Double CV for model selection and assessment        #
###################################################################
##### model assessment OUTER CV(10)  - Initialization #####
# Switch out male/female label with 0/1
voice$gender = 0
voice$gender[voice$label == "male"] = 1
voice$gender = as.factor(voice$gender)
voice = voice[-21]

xy.out = voice
n.out = dim(xy.out)[1]
allpredictedCV.out = rep(NA,n.out)
k.out = 10
nmodels = 3
K = seq(1,31, by=2) # set potential Ks for Knn, being sure to make choices odd to avoid ties.

#define the cross-validation splits 
groups.out = c(rep(1:k.out,floor(n.out/k.out)),1:(n.out%%k.out))  #produces list of group labels

# Added this line due to wonkiness in the line above. One of groups.out was still zero.
groups.out = groups.out[groups.out > 0]

# Set seed for re-creation of results, create group membership for outer folds.
set.seed(100)
cvgroups.out = sample(groups.out,n.out)

###############################################
##### model assessment OUTER shell CV(10) #####
###############################################
for (j in 1:k.out)  { 
  groupj.out = (cvgroups.out == j)
  
  # define the training and validation set for outer loop
  trainxy.out = xy.out[!groupj.out,]
  testxy.out = xy.out[groupj.out,]
  
  # Perform separation of response and predictors 
  train.x.out = trainxy.out[,-21]
  train.y.out = trainxy.out[, 21]
  test.x.out = testxy.out[-21]
  test.y.out = testxy.out[, 21]
  
  # Perform scaling necessary for knn
  train.x.out.std = scale(train.x.out)
  test.x.out.std = scale(test.x.out, center = attr(train.x.out.std, "scaled:center"), 
                         scale = attr(train.x.out.std, "scaled:scale"))
  
  ##############################################
  ###   model selection on trainxy.out       ###
  ##############################################
  xy.in = trainxy.out  # take outer training set and run inner CV loop for model selection
  
  n.in = dim(xy.in)[1]
  ncv = 10
  if ((n.in%%ncv) == 0) {
    groups.in= rep(1:ncv,floor(n.in/ncv))} else {
      groups.in=c(rep(1:ncv,floor(n.in/ncv)),(1:(n.in%%ncv)))
    }
  
  # Create group membership for outer folds.
  cvgroups.in = sample(groups.in,n.in)
  allpredictedcv10.in = matrix(rep(0,n.in*nmodels),ncol=nmodels)

  for (i in 1:ncv) {
    
    # define the training and validation set for inner loop
    newdata.in = xy.in[cvgroups.in==i,]
    
    train.x.in = xy.in[cvgroups.in != i,-21]
    train.y.in = xy.in[cvgroups.in != i, 21]
    test.x.in = xy.in[cvgroups.in == i,-21]
    test.y.in = xy.in[cvgroups.in == i, 21]
    
    # Perform scaling necessary for knn
    train.x.in.std = scale(train.x.in)
    test.x.in.std = scale(test.x.in, center = attr(train.x.in.std, "scaled:center"), 
                          scale = attr(train.x.in.std, "scaled:scale"))
    
    # Model 1 - Logistic regression less meanfun, modindx, and minfun
    log32fit = glm(gender ~ . -meanfun-modindx-minfun, data=xy.in, 
                   subset=(cvgroups.in!=i), family=binomial)
    log32prob = predict(log32fit,newdata.in,type="response")
    log32fact = rep(0,dim(newdata.in)[1])
    # Set > 50% certainty threshold for predicted classification
    log32fact[log32prob > 0.5] = 1
    allpredictedcv10.in[cvgroups.in==i,1] = log32fact
    
    # Model 2 - Logistic regression, all predictors
    log3fit = glm(gender ~ ., data=xy.in, subset=(cvgroups.in!=i), family=binomial)
    log3prob = predict(log3fit,newdata.in,type="response")
    log3fact = rep(0,dim(newdata.in)[1])
    # Set > 50% certainty threshold for predicted classification
    log3fact[log3prob > 0.5] = 1
    allpredictedcv10.in[cvgroups.in==i,2] = log3fact
    
    # Model 3 - model setup, predictions, and CV calculation
    err.rate.in = rep(NA, length(K))
    
    # First, find the optimal value of k for this one of ten *INNER* splits of
    # train/test.
    for (k.in in 1:length(K)) {
      knn_prob.in = knn(train.x.in.std, test.x.in.std, train.y.in, k = K[k.in])
      mytable.in = table(knn_prob.in, test.y.in)
      err.rate.in[k.in] = 1 - (sum(diag(mytable.in))/sum(mytable.in))
    }
    
    # Which value of k resulted in the lowest error rate for this pass?
    whichK.in = order(err.rate.in)[1]
    bestK.in = K[whichK.in]
    
    # Run the k-nn with the best-k to get the post optimal prediction set for
    # this inner pass.
    knn_prob.in = knn(train.x.in.std, test.x.in.std, train.y.in, k = bestK.in)
    knn_fact.in = as.numeric(as.character(knn_prob.in))
    allpredictedcv10.in[cvgroups.in==i,3] = knn_fact.in
  }
  
  # Determine which is the best model by taking the one with the lowest overall
  # CV10 score.
  allcv10.in = rep(0,nmodels)
  for (m in 1:nmodels) allcv10.in[m] = sum(xy.in$gender!=allpredictedcv10.in[,m])/n.in 
  bestmodels = (1:nmodels)[allcv10.in == min(allcv10.in)]
  #################################################################################
  ###   resulting in bestmodels - prepare data for assessment phase             ###
  #################################################################################
  
  # If more than one model receives the same CV10 score, pick one at random.
  bestmodel = ifelse(length(bestmodels)==1,bestmodels,sample(bestmodels,1))
  
  
  if (bestmodel == 1)  {
    # Generate predictions for glm over all but three predictors using the outer CV fold scheme.
    log32fit.train = glm(gender ~ . -meanfun-modindx-minfun, data= trainxy.out, family=binomial)
    log32prob.test = predict(log32fit.train,testxy.out,type="response")
    predictvalid = rep(0,dim(testxy.out)[1])
    # Set > 50% certainty threshold for predicted classification
    predictvalid[log32prob.test > 0.5] = 1
  }
  
  if (bestmodel == 2)  {
    # Generate predictions for glm over all predictors using the outer CV fold scheme.
    log3fit.train = glm(gender ~ ., data= trainxy.out, family=binomial)
    log3prob.test = predict(log3fit.train,testxy.out,type="response")
    predictvalid = rep(0,dim(testxy.out)[1])
    # Set > 50% certainty threshold for predicted classification
    predictvalid[log3prob.test > 0.5] = 1
  }
  
  if (bestmodel == 3)  {
    # Generate predictions for knn over all predictors using the outer CV fold scheme.
    
    err.rate.out = rep(NA, length(K))
    
    # Find the optimal value of k for this one of ten *OUTER* splits of train/test
    #k.out = 1
    for (k.out in 1:length(K)) {
      knn_prob.out = knn(train.x.out.std, test.x.out.std, train.y.out, k = K[k.out])
      mytable.out = table(knn_prob.out, test.y.out)
      err.rate.out[k.out] = 1 - (sum(diag(mytable.out))/sum(mytable.out))
    }
    
    # Which value of k resulted in the lowest error rate for this pass?
    whichK.out = order(err.rate.out)[1]
    bestK.out = K[whichK.out]
    
    knn_prob.out = knn(train.x.out.std, test.x.out.std, train.y.out, k = bestK.out)
    knn_fact.out = as.numeric(as.character(knn_prob.out))
    predictvalid = knn_fact.out
  }
  allpredictedCV.out[groupj.out] = predictvalid
}

####################
# Assessment phase #
####################
# 1. Model accuracy scores

sprintf("Model accuracy scores")
sprintf("Model 1: %f Model 2: %f Model 3: %f", allcv10.in[1],allcv10.in[2],allcv10.in[3])

# 2. Prediction table and overall predictive results (p.out for classification
# instead of R^2 for regression.)

final_table = table(Actual = voice$gender, Predicted = allpredictedCV.out)
p.out = sum(voice$gender != allpredictedCV.out)/n.out

sprintf("Overall Predictive Accuracy is %.2f percent, or (%d+%d)/%d", 
        100*(1-p.out), final_table[1,1], final_table[2,2], sum(final_table))

# 3. Interpretation of prediction table.
print(final_table)

sprintf("Correctly predicted female?: %.2f percent, or %d/(%d+%d)", 
        100*(final_table[1,1] /(final_table[1,1] + final_table[1,2])),
        final_table[1,1], final_table[1,1] ,final_table[1,2])

sprintf("Correctly predicted male?: %.2f percent, or %d/(%d+%d)", 
        100*(final_table[2,2] /(final_table[2,2] + final_table[2,1])),
        final_table[2,2], final_table[2,2], final_table[2,1])

sprintf("Female voice predicted correctly?: %.2f percent, or %d/(%d+%d)", 
        100*(final_table[1,1] /(final_table[1,1] + final_table[2,1])),
        final_table[1,1], final_table[1,1] ,final_table[2,1])

sprintf("Male voice predicted correctly?: %.2f percent, or %d/(%d+%d)", 
        100*(final_table[2,2] /(final_table[2,2] + final_table[1,2])),
        final_table[2,2], final_table[2,2], final_table[1,2])

plot(K, err.rate.out, type = "o", main="Choice of best k for KNN in validation set")
points(K[whichK.out], err.rate.out[whichK.out], col="blue", pch=19, cex=2)
