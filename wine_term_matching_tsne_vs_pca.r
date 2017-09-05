# initialize all necessary packages
options(warn=-1)

suppressMessages(library(dplyr))
suppressMessages(library(slam))
suppressMessages(library(lsa))
suppressMessages(library(tm))
suppressMessages(library(NLP))
suppressMessages(library(ggplot2))
suppressMessages(library(gmodels))
suppressMessages(library(reshape2))
suppressMessages(library(SnowballC))
suppressMessages(library(Rtsne))



######################################################
#first, load the wine terms we're going to need

wine_terms<-t(read.csv("wine_terms.txt", sep = ",", header = F))
match_list<-lapply(wine_terms, function(x) (grep(x,wine_terms)))
ctr = 0
matched_terms = list()
matched_len <- c()
                       
for(ii in match_list){
 if(length(match_list[ii])>1){
   ctr = ctr+1
   
   matched_terms[ctr] <- match_list[ii]

    }
}

matched_terms.select = list()
ctr = 0

match_order<-order(lengths(matched_terms),decreasing = TRUE)
all_term_index = c(1:nrow(wine_terms))

for(ii in match_order){
  
  if (sum(is.element(unlist(matched_terms[ii]),c(all_term_index)))>1){
    ctr = ctr+1
    
      all_term_index <-setdiff(all_term_index, unlist(matched_terms[ii]))
  matched_terms.select[ctr] <- matched_terms[ii]
  
  }
  
  
}
          
#######################################################
# then, load the review data, and bind into a dataframe

wine_cellar <- readRDS("wine_collection_V2.RDS")
wine_all <- bind_rows(wine_cellar)

########################################################################
# then, let's define a function to clean and match reviews to wine terms

clean_review = function(x,wine_terms){
  
  review_terms <- x %>% 
    toupper() %>% 
    removePunctuation() %>% 
    strsplit(" ")
  
  review_vector <- matrix(unlist(review_terms), ncol = 1, byrow = TRUE)
  term_vector <- as.numeric(wine_terms%in%review_vector)
  return(term_vector)
  
}

####################################################
# let's subset to remove infrequent wines
# and filter out others, if we want
# (e.g. remove blends, keep just one wine type, etc)

wine_all.new <- wine_all

wine_all.new <- wine_all.new %>% group_by(Variety) %>% filter(n() >= 50) %>% ungroup() %>% as.data.frame()
wine_all.new <- wine_all.new %>% filter(!grepl("Portuguese",wine_all.new$Variety, fixed=TRUE))

plot(sort(table(wine_all.new$Variety)))


####################################################
# then, let's use that function on all wine reviews
# by first picking out the reviews

wine_reviews <- wine_all.new["review"]
wine_reviews <- split(wine_reviews,seq(nrow(wine_reviews)))

# then by snatching the IDs for each wine
wine_IDs <- wine_all.new["Variety"]
wine_IDs <- unlist(split(wine_IDs, seq(nrow(wine_IDs))))

# then using the function on each review
term_matrix <- lapply(wine_reviews, function(x) clean_review(x,wine_terms))
term_matrix <- matrix(unlist(term_matrix), ncol = nrow(wine_all.new), dimnames = list(NULL, wine_IDs))

# # once we have our matrix - let's combine data from terms with common stems
# # e.g. tannins and tannic
# new_term_matrix<-matrix(data = 0, ncol = nrow(wine_all.new), nrow = length(all_term_index+length(matched_terms.select)),dimnames = list(NULL, wine_IDs))
# 
# # first, compile all terms with unique stems (e.g. stems with just one term)
# ctr=0
# for (ii in c(1:nrow(term_matrix))){
#   if (is.element(ii,all_term_index)){
#     ctr <-ctr+1
#     new_term_matrix[ctr,]<-term_matrix[ii,]
#   }
# }
# 
# # next, compile all terms sharing a stem
# 
# # for each shared stem
# for (ii in c(1:length(matched_terms.select))){
#   ctr <-ctr+1
# 
#   # for each review
#   for (jj in c(1:ncol(new_term_matrix))){
# 
#     # if one of the terms in the stem is found in the review
#     if(sum(term_matrix[unlist(matched_terms.select[ii]),jj])>1){
# 
#       #mark the stem as found in the review
#     new_term_matrix[ctr,jj]<-1
# 
#     }
#   }
# }

###############################################################################
## OK.  So we've got ~220 features (terms) to judge wine similarity
## can we use these to break down wine varieties in an interpretable way? 
## AND DOES THIS LOOK LIKE THE UNSTEMMED PCA?

## to simplify the 220, let's do PCA on that new term matrix
pc_results <- prcomp(term_matrix) 

tsne_results <- Rtsne(t(term_matrix), check_duplicates = FALSE)
tsn_bound <- cbind(as.data.frame(tsne_results$Y),wine_all.new)

xyz<-ggplot(data = tsn_bound, aes(x = V1, y = V2, color = Category)) +geom_point()
plot(xyz)

## and bind wine features to the PCS for easy sorting 
pcs <- cbind(pc_results$rotation,wine_all.new)
pc1_wineload = aggregate(pcs$PC1, by= list(pcs$Variety), FUN = mean)
pc2_wineload = aggregate(pcs$PC1, by= list(pcs$Variety), FUN = mean)

## now let's structure a plot on the basis of wine types -- 
#perhaps red vs white vs rose?
plt1 <- ggplot(subset(pcs,Variety %in% c("White Blend","Rosé", "Red Blend")), 
               aes(x = PC1, y = PC2, colour = Variety)) + geom_point( alpha = 1/2) + guides(col = guide_legend(nrow = 2)) + theme(legend.position = "bottom")+ xlim(-.004, .004) + ylim(-.0041, .025)

#the big grapes?  
plt2 <- ggplot(subset(pcs,Variety %in% c("Chardonnay",  "Sauvignon Blanc","Cabernet Sauvignon","Sangiovese")), 
               aes(x = PC1, y = PC2, colour = Variety)) + geom_point( alpha = 1/2)  + guides(col = guide_legend(nrow = 2))+  theme(legend.position = "bottom") + xlim(-.004, .0044) + ylim(-.0041, .025)

#some little grapes?
plt3 <- ggplot(subset(pcs,Variety %in% c("Grüner Veltliner", "Petit Verdot", "Viognier", "Cabernet Franc","Malbec", "Nebbiolo")), 
               aes(x = PC1, y = PC2, colour = Variety)) + geom_point( alpha = 1/2) + guides(col = guide_legend(nrow = 2))+  theme(legend.position = "bottom") + xlim(-.004, .0044) + ylim(-.0041, .025)

#desserts, ports and bubbly?
plt4 <- ggplot(subset(pcs,Category %in% c("Dessert", "Sparkling","Port/Sherry")), 
               aes(x = PC1, y = PC2, colour = Category)) + geom_point( alpha = 1/2)  + guides(col = guide_legend(nrow = 2))+ theme(legend.position = "bottom") + xlim(-.004, .0044) + ylim(-.0041, .025)


## now let's structure a plot on the basis of wine types -- 
#perhaps red vs white vs rose?
tplt1 <- ggplot(subset(tsn_bound ,Variety %in% c("White Blend","Rosé", "Red Blend")), 
               aes(x = V1, y = V2, colour = Variety)) + geom_point( alpha = 1/2) + guides(col = guide_legend(nrow = 2)) + theme(legend.position = "bottom")

#the big grapes?  
tplt2 <- ggplot(subset(tsn_bound,Variety %in% c("Chardonnay",  "Sauvignon Blanc","Cabernet Sauvignon","Sangiovese")), 
               aes(x = V1, y = V2, colour = Variety)) + geom_point( alpha = 1/2)  + guides(col = guide_legend(nrow = 2))+  theme(legend.position = "bottom")

#some little grapes?
tplt3 <- ggplot(subset(tsn_bound,Variety %in% c("Grüner Veltliner", "Petit Verdot", "Viognier", "Cabernet Franc","Malbec", "Nebbiolo")), 
               aes(x = V1, y = V2, colour = Variety)) + geom_point( alpha = 1/2) + guides(col = guide_legend(nrow = 2))+  theme(legend.position = "bottom") 

#desserts, ports and bubbly?
tplt4 <- ggplot(subset(tsn_bound,Category %in% c("Dessert", "Sparkling","Port/Sherry")), 
               aes(x = V1, y = V2, colour = Category)) + geom_point( alpha = 1/2)  + guides(col = guide_legend(nrow = 2))+ theme(legend.position = "bottom")


source("multiplot.r")
multiplot(cols = 2, plt1,plt2,plt3,plt4)


source("multiplot.r")
multiplot(cols = 2, tplt1,tplt2,tplt3,tplt4)

#wine_terms[order(pc_results$x[,1])]
#wine_terms[order(pc_results$x[,2])]

#####################################################################
# well, now that we've got features and data - let's try and classify
# but.. perhaps on just a subset of the data? e.g. classes with sufficient samples?

library(mlr)

pcs2 <- pcs %>% group_by(Variety) %>% filter(n() >= 300) %>% ungroup() %>% as.data.frame()
pcs2 <- pcs2 %>% filter(!grepl("Blend",pcs2$Variety, fixed=TRUE))




tns2 <- tsn_bound %>% group_by(Variety) %>% filter(n() >= 300) %>% ungroup() %>% as.data.frame()
tns2 <- tns2 %>% filter(!grepl("Blend",tns2$Variety, fixed=TRUE))


#mldata = cbind(pcs2[,1:150],pcs2[,239])
mldata = cbind(pcs2[,1:2],pcs2[,296])

colnames(mldata)[3] <- "Variety"


## 1) Define the task
## Specify the type of analysis (e.g. classification) and provide data and response variable
task = makeClassifTask(data = mldata, target = "Variety")

## 2) Define the learner
## Choose a specific algorithm (e.g. linear discriminant analysis)
lrn = makeLearner("classif.svm") #svm and ksvm best, then cforest, next glmnet then xgboost good too

n = nrow(mldata)
train.set = sample(n, size = 2/3*n)
test.set = setdiff(1:n, train.set)

## 3) Fit the model
## Train the learner on the task using a random subset of the data as training set
model = train(lrn, task, subset = train.set)

## 4) Make predictions
## Predict values of the response variable for new observations by the trained model
## using the other part of the data as test set
pred_pc = predict(model, task = task, subset = test.set)

## 5) Evaluate the learner
## Calculate the mean misclassification error and accuracy
performance(pred_pc, measures = list(mmce, acc, ber, kappa))

predconfmat <- calculateConfusionMatrix(pred = pred_pc)

## before plotting we need to scale the data (to the percent of wines classified per bin per column)
#predconfmat$result <- t(scale(t(predconfmat$result), center = FALSE, scale = TRUE))
predconfmat$result <- t(apply(predconfmat$result, 1, function(x)100*(x-min(x))/(max(x)-min(x))))


## now sort those wines in a sensible way
pc_wineload = aggregate(pcs2$PC1+pcs2$PC2, by= list(pcs2$Variety), FUN = mean)

image_pc = qplot(x = true, y = predicted, fill=value, data=(melt(predconfmat$result[order(pc_wineload$x),order(pc_wineload$x)])),geom="tile")+theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))
plot(image)


#mldata = cbind(pcs2[,1:150],pcs2[,239])
mldata = cbind(tns2[,1:2],pcs2[,296])

colnames(mldata)[3] <- "Variety"


## 1) Define the task
## Specify the type of analysis (e.g. classification) and provide data and response variable
task = makeClassifTask(data = mldata, target = "Variety")

## 2) Define the learner
## Choose a specific algorithm (e.g. linear discriminant analysis)
lrn = makeLearner("classif.svm") #svm and ksvm best, then cforest, next glmnet then xgboost good too

n = nrow(mldata)
train.set = sample(n, size = 2/3*n)
test.set = setdiff(1:n, train.set)

## 3) Fit the model
## Train the learner on the task using a random subset of the data as training set
model = train(lrn, task, subset = train.set)

## 4) Make predictions
## Predict values of the response variable for new observations by the trained model
## using the other part of the data as test set
pred_tn = predict(model, task = task, subset = test.set)

## 5) Evaluate the learner
## Calculate the mean misclassification error and accuracy
performance(pred_tn, measures = list(mmce, acc, ber, kappa))

predconfmat <- calculateConfusionMatrix(pred = pred_tn)

## before plotting we need to scale the data (to the percent of wines classified per bin per column)
#predconfmat$result <- t(scale(t(predconfmat$result), center = FALSE, scale = TRUE))
predconfmat$result <- t(apply(predconfmat$result, 1, function(x)100*(x-min(x))/(max(x)-min(x))))


## now sort those wines in a sensible way
pc_wineload = aggregate(pcs2$PC1+pcs2$PC2, by= list(pcs2$Variety), FUN = mean)

image_tn = qplot(x = true, y = predicted, fill=value, data=(melt(predconfmat$result[order(pc_wineload$x),order(pc_wineload$x)])),geom="tile")+theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))
multiplot(cols = 2,image_pc,image_tn)

#########################################################################################
## ok - classification works, and when it fails, the model generalizes in a sensible way
## but can we assess multiple learners on a range of tasks? 

##
# a dataset based on big wine varieties

pcs2 <- pcs %>% group_by(Variety) %>% filter(n() >= 300) %>% ungroup() %>% as.data.frame()
pcs2 <- pcs2 %>% filter(!grepl("Blend",pcs2$Variety, fixed=TRUE))

mldata_big_varieties = cbind(pcs2[,1:50],pcs2[,296])
colnames(mldata_big_varieties)[51] <- "Target"

##
# a dataset based on most wine varieties

pcs2 <- pcs %>% group_by(Variety) %>% filter(n() >= 50) %>% ungroup() %>% as.data.frame()
pcs2 <- pcs2 %>% filter(!grepl("Blend",pcs2$Variety, fixed=TRUE))

mldata_small_varieties = cbind(pcs2[,1:50],pcs2[,296])
colnames(mldata_small_varieties)[51] <- "Target"

##
# a dataset based on wine category

pcs2 <- pcs %>% group_by(Variety) %>% filter(n() >= 50) %>% ungroup() %>% as.data.frame()
pcs2 <- subset(pcs2,Category %in% c("White", "Rosé", "Red"))

mldata_wine_category = cbind(pcs2[,1:50],pcs2[,297])
colnames(mldata_wine_category)[51] <- "Target"

## 1) Define the tasks
# we now need several tasks on which to assess the accuracy of our learners
# by defining a list of tasks to run through

tasks = list(makeClassifTask(data = mldata_big_varieties, target = "Target"),
            makeClassifTask(data = mldata_small_varieties, target = "Target"),
            makeClassifTask(data = mldata_wine_category, target = "Target")
            )

## 2) Define the learner
## Choose a specific algorithm (e.g. linear discriminant analysis)
lrns = list(makeLearner("classif.randomForest"),
            makeLearner("classif.svm"),
           makeLearner("classif.xgboost"),
           makeLearner("classif.lda"),
           makeLearner("classif.naiveBayes") 
          )


## 3) decide a resampling strategy
# (e.g. holdout, CV)

rdesc = makeResampleDesc("CV", iter=10)

## 4) choose what measures to evaluate on
# eg. meas = list(mmce, ber, timetrain)

meas = list(mmce, acc, ber, timetrain)


## 5) conduct the benchmarking experiment

bmr = benchmark(lrns, tasks, rdesc, measures = meas)

## 6) Evaluate the learners on all tasks

# on the basis of balanced error rate (mean classification error across groups)
plotBMRBoxplots(bmr, measure = ber, style = "violin", pretty.names = TRUE) +
  aes(color = learner.id) +
  theme(strip.text.x = element_text(size = 12))

# and on the basis of time it takes to train the model
plotBMRBoxplots(bmr, measure = timetrain, style = "violin", pretty.names = TRUE) +
  aes(color = learner.id) +
  theme(strip.text.x = element_text(size = 12))

#########################################################################################
## ok - we can classify AND have a sense of what works best
## now, how many dimensions are required to really reach this level of effective classification
## or, to put it another way: what's the dimensionality of wine taste ?
## while many of the terms used to describe wines covary, and some of the terms 
## reliably distingish wines quite well (e.g. cherry, tannins, pinapple) 
## we have ~270 potential terms to describe wines by -- how many actually contribute 
## to successfully telling the difference amongst wines by category or variety? 

## to get a sense of the dimensionality - let's look at the eigenvalues
plot(pc_results$sdev/sum(pc_results$sdev))

## and let's look at how those eigenvalues sum to % total variance explained

plot(cumsum(pc_results$sdev/sum(pc_results$sdev)))

## how does this relate to our ability to classify? 
# let's make a function to ease fitting a few things

fit_pca_data<-function(pcs2, group2fit, niter, npcs){
  
  pcs_ctr <- 0
  class_performance = matrix(data=NA, nrow=length(npcs), ncol=niter)
  
  
  for (numpcs in npcs){
    pcs_ctr <-pcs_ctr+1
    
    for (niter in c(1:niter)){
      
      if (group2fit == "Variety"){
        mldata <- cbind(pcs2[,1:numpcs],pcs2[,296])
        colnames(mldata)[numpcs+1] <- "Target"
      }
      
      if (group2fit == "Category"){
        mldata <- cbind(pcs2[,1:numpcs],pcs2[,297])
        colnames(mldata)[numpcs+1] <- "Target"
      }
      
      if (numpcs == 1){
        colnames(mldata)[1:numpcs] <- "pcs"
        
        mldata <- as.data.frame(mldata)
      }
      
      # set up the classification via MLR: task, learner, subsetting, model, prediction
      task = makeClassifTask(data = mldata, target = "Target")
      lrn = makeLearner("classif.svm") #svm and ksvm best, then cforest, next glmnet then xgboost good too
      
      n = nrow(mldata)
      train.set = sample(n, size = 2/3*n)
      test.set = setdiff(1:n, train.set)
      
      model = train(lrn, task, subset = train.set)
      pred = predict(model, task = task, subset = test.set)
      
      ## 5) Evaluate the learner
      ## Calculate the mean misclassification error and accuracy
      class_performance[pcs_ctr,niter] <- performance(pred, measures = acc)
    }
  }
  return(class_performance)
}


# and now let's fit some stuff


niter = 3
npcs = c(1,2,3,4,6,8,10,15,20,30,40,50,75,100,125,150,200,225,250)


# like the most common wines 

pcs2 <- pcs %>% group_by(Variety) %>% filter(n() >= 300) %>% ungroup() %>% as.data.frame()
pcs2 <- pcs2 %>% filter(!grepl("Blend",pcs2$Variety, fixed=TRUE))
group2fit = "Variety"

class_biggrapes_performance <- fit_pca_data(pcs2, group2fit, niter, npcs )

# or lesser wines 

pcs2 <- pcs %>% group_by(Variety) %>% filter(n() >= 50) %>% ungroup() %>% as.data.frame()
pcs2 <- pcs2 %>% filter(!grepl("Blend",pcs2$Variety, fixed=TRUE))
group2fit = "Variety"

class_smallgrapes_performance <- fit_pca_data(pcs2, group2fit, niter, npcs )


# or or wine categories

pcs2 <- pcs %>% group_by(Variety) %>% filter(n() >= 50) %>% ungroup() %>% as.data.frame()
pcs2 <- subset(pcs2,Category %in% c("White", "Red"))
group2fit = "Category"

class_category_performance <- fit_pca_data(pcs2, group2fit, niter, npcs )

## and then aggregate the measures of interest

class_category_performance_t = as.data.frame(class_category_performance)
class_nmean <- apply(class_category_performance_t,1,function(x) mean(na.omit(x)))
class_nmean <- class_nmean/max(class_nmean)
class_sd <- apply(class_category_performance_t,1,function(x) sd(na.omit(x)/max(class_nmean)))
xdata1 = as.data.frame(cbind(npcs,class_nmean,class_sd))

class_biggrapes_performance_t = as.data.frame(class_biggrapes_performance)
class_nmean2 <- apply(class_biggrapes_performance_t,1,function(x) mean(na.omit(x)))
class_nmean2 <- class_nmean2/max(class_nmean2)
class_sd2 <- apply(class_biggrapes_performance_t,1,function(x) sd(na.omit(x)/max(class_nmean2)))
xdata2 = as.data.frame(cbind(npcs,class_nmean2,class_sd2))

class_smallgrapes_performance_t = as.data.frame(class_smallgrapes_performance)
class_nmean3 <- apply(class_smallgrapes_performance_t,1,function(x) mean(na.omit(x)))
class_nmean3 <- class_nmean3/max(class_nmean3)
class_sd3 <- apply(class_smallgrapes_performance_t,1,function(x) sd(na.omit(x)/max(class_nmean3)))
xdata3 = as.data.frame(cbind(npcs,class_nmean3,class_sd3))

xdata = Reduce(merge, list(xdata1, xdata2, xdata3))

## and plot the classifier output

plt1 <- ggplot(xdata) + 
  geom_line(aes(x = npcs, y = class_nmean, color = "c1"), size = 1) +
  geom_ribbon(aes(x = npcs, ymin = class_nmean - 2*class_sd, ymax = class_nmean + 2*class_sd), alpha = 0.3) +

  geom_line(aes(x = npcs, y = class_nmean2, color = "c2"), size = 1) +
  geom_ribbon(aes(x = npcs, ymin = class_nmean2 - 2*class_sd2, ymax = class_nmean2 + 2*class_sd2), alpha = 0.3) +


  geom_line(aes(x = npcs, y = class_nmean3, color = "c3"), size = 1) +
  geom_ribbon(aes(x = npcs, ymin = class_nmean3 - 2*class_sd3, ymax = class_nmean3 + 2*class_sd3), alpha = 0.3) +
  
  
  xlab('number of PCs')+ylab('fracton of total classifcation') + 
  
  scale_colour_manual(name = 'Classification of:', 
                      values =c('c1'='Blue','c2'='Orange','c3'='Red'), labels = c('Category','6 Big Varieties','All Varieties'))+
  
  scale_x_log10(breaks = npcs[c(1:10,12,14,16,19)], limits = c(.9, 251))


 

plot(plt1)

