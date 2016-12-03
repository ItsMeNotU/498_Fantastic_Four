# =======================================================================
# Comparing several models for overview of performance (binary helpful)
# GBM, SVM, XGB, RF, C5.0 best performers
# Candidates for ensemble
# =======================================================================

# Matching column indices to keep for the dataset passed to the algorithm
ToMatch = c("overall.num", "reviewText.count", "summary.count",
            "date.diff.num","dev.mean","all.caps.ratio",
            "n.punc","Total.Reviews","avg.word.len","time.min.num",
            "helpful.binary")
Cols = grep(paste(ToMatch,collapse = "|"), colnames(reviews.mod.trn.sub))

# prepare training scheme
control <- trainControl(method="cv", number=5)

cl <- makeCluster(detectCores())
registerDoParallel(cl)
# train the GBM model
set.seed(7)
modelGbm <- train(helpful.binary~., data=reviews.mod.trn.sub[,Cols], 
                  method="gbm", 
                  trControl=control, 
                  verbose=FALSE)

# train the SVM model
set.seed(7)
modelSvm <- train(helpful.binary~., data=reviews.mod.trn.sub[,Cols], 
                  method="svmRadial", 
                  trControl=control)

# train the xgb tree
set.seed(7)
modelXGB <- train(helpful.binary~., data=reviews.mod.trn.sub[,Cols], 
                  method="xgbTree", 
                  trControl=control)

# train the naive bayes
set.seed(7)
modelNB <- train(helpful.binary~., data=reviews.mod.trn.sub[,Cols], 
                 method="nb", 
                 trControl=control)

# train the rf
set.seed(7)
modelRF <- train(helpful.binary~., data=reviews.mod.trn.sub[,Cols], 
                 method="rf", 
                 trControl=control)

#train C5.0
set.seed(7)
modelC5.0 <- train(helpful.binary~., data=reviews.mod.trn.sub[,Cols], 
                   method="C5.0", 
                   trControl=control)

#train Neural Net
set.seed(7)
modelNNET <- train(helpful.binary~., data=reviews.mod.trn.sub[,Cols], 
                   method="nnet", 
                   trControl=control)

stopCluster(cl)

# collect resamples
results <- resamples(list(GBM=modelGbm, SVM=modelSvm, XGB=modelXGB,
                          NB = modelNB, RF=modelRF,C5.0=modelC5.0,
                          NNET= modelNNET))

# summarize the distributions
summary(results)

# =======================================================================
# Ensemble Model
# Base Learners: SVM, RF, C5.0
# Second level model: XGB
# =======================================================================

# Converting helpful bins to a binary variable
reviews.mod.trn.sub$helpful.binary = ifelse(reviews.mod.trn.sub$helpful.bins=="Upper", "Helpful","Not_Helpful")
reviews.mod.trn$helpful.binary = ifelse(reviews.mod.trn$helpful.bins=="Upper", "Helpful","Not_Helpful")
reviews.mod.tst$helpful.binary = ifelse(reviews.mod.tst$helpful.bins=="Upper", "Helpful","Not_Helpful")

#subset columns from review.mod.trn & review.mod.tst
ToMatch = c("overall.num", "reviewText.count", "summary.count",
            "date.diff.num","dev.mean","all.caps.ratio",
            "n.punc","Total.Reviews","avg.word.len","time.min.num",
            "helpful.binary")
Cols = grep(paste(ToMatch,collapse = "|"), colnames(reviews.mod.trn.sub))

train = reviews.mod.trn[, Cols]
test = reviews.mod.tst[,Cols]

head(train)
head(test)

#set seed for reproducability
set.seed(1234)

#Create ensembleData & blenderData
train = train[sample(nrow(train)),]
split = floor(nrow(train)/2)

ensembleData = train[0:split,]
blenderData = train[(split + 1):(split*2), ]

#assign label name & predictor variables
labelName = "helpful.binary"
predictors = names(ensembleData)[names(ensembleData) != labelName]

#set train control 
myControl = trainControl(method = "cv", number = 3, returnResamp = 'none',
                         classProbs = TRUE)

#train first level models
cl <- makeCluster(detectCores())
registerDoParallel(cl)

model.C5.0 = train(ensembleData[,predictors], ensembleData[,labelName], 
                  method = "C5.0", trControl = myControl)

model.svm <- train(ensembleData[,predictors], ensembleData[,labelName], 
                  method = "svmRadial", trControl = myControl)

model.rf <- train(ensembleData[,predictors], ensembleData[,labelName], 
                 method="rf", trControl=control)

stopCluster(cl)

# predict on the blender data & testing data, probabilities that are added
# are those that predic helpful 
# Blender data
# gbm
C5.0.preds = predict(object = model.C5.0, blenderData[,predictors], type = "prob")
blenderData$C5.0.probs = C5.0.preds$Helpful

# svm
svm.preds = predict(object = model.svm, blenderData[,predictors], type = "prob")
blenderData$svm.probs = svm.preds$Helpful

# rf
rf.preds = predict(object = model.rf, blenderData[,predictors], type = "prob")
blenderData$rf.probs = rf.preds$Helpful


# Testing data
# gbm
C5.0.preds = predict(object = model.C5.0, test[,predictors], type = "prob")
test$C5.0.probs = C5.0.preds$Helpful

# svm
svm.preds = predict(object = model.svm, test[,predictors], type = "prob")
test$svm.probs = svm.preds$Helpful

# rf
rf.preds = predict(object = model.rf, test[,predictors], type = "prob")
test$rf.probs = rf.preds$Helpful

head(blenderData)

#predictors for the second level model
predictors = names(blenderData)[names(blenderData) != labelName]

cl <- makeCluster(detectCores())
registerDoParallel(cl)

x =  data.matrix(blenderData[,predictors])
y = as.numeric(as.factor(blenderData[,labelName])) - 1

dtrain <- xgb.DMatrix(data = x, label = y)

ptm = proc.time()
xgb.binary = xgboost(data = dtrain,
                     eta = 0.01,
                     max_depth = 15, 
                     nround=250, 
                     objective="binary:logistic",
                     nthread = 5
)
proc.time() - ptm; rm(ptm)

preds = predict(xgb.binary, data.matrix(test[,predictors]))

preds <- ifelse(preds>.50, "Not_Helpful","Helpful")

cm = confusionMatrix(preds, test$helpful.binary)

cm
                 