# ==================================================
# xgboost model predicting helpfulness as binary reponse
# Accuracy: .71
# Use the probabilities as a way to rank the reviews shown to the customer
# Higher probability means a higher confidence that the review is "helpful"
# System will show reviews with high probability of helpfulness
# ==================================================

getwd()

#load data
load('/Users/tobiamartens/Desktop/reviews.RData')

# Converting helpful bins to a binary variable
reviews.mod.trn.sub$helpful.binary = ifelse(reviews.mod.trn.sub$helpful.bins=="Upper", "Helpful","Not_Helpful")
reviews.mod.trn$helpful.binary = ifelse(reviews.mod.trn$helpful.bins=="Upper", "Helpful","Not_Helpful")
reviews.mod.tst$helpful.binary = ifelse(reviews.mod.tst$helpful.bins=="Upper", "Helpful","Not_Helpful")

# create character vector to match predictor column names
ToMatch = c("overall.num", "reviewText.count",
            "date.diff.num","dev.mean","all.caps.ratio","avg.asin.rating",
            "n.punc","Total.Reviews","avg.word.len","time.min.num")

predictor_cols = grep(paste(ToMatch,collapse = "|"), colnames(reviews.mod.trn))

# match response column index
response_col = grep("helpful.binary", colnames(reviews.mod.trn))

library(xgboost)


x = data.matrix(reviews.mod.trn[,predictor_cols])

y = as.numeric(as.factor(reviews.mod.trn[,response_col])) - 1

dtrain <- xgb.DMatrix(data = x, label = y)

# fit xgboost model
ptm = proc.time()
xgb.binary = xgboost(data = dtrain,
              eta = 0.1,
              max_depth = 15, 
              nround=200, 
              objective="binary:logistic",
              nthread = 5
)
proc.time() - ptm; rm(ptm)

# Process test data for GBM
reviews.mod.tst$time.min.num = as.numeric(as.POSIXct(reviews.mod.tst$time.min))
reviews.mod.tst$date.diff.num = as.numeric(reviews.mod.tst$date.diff)

# predict values in test set
xgb.pred.binary = predict(xgb.binary, data.matrix(reviews.mod.tst[,predictor_cols]))

# add probabilities as a column to the test data set
reviews.mod.tst$xgb.pred.binary = xgb.pred.binary

# sort the test reviews from most confident 
sort.reviews.mod.tst = reviews.mod.tst[order(xgb.pred.binary),]

# view 
head(sort.reviews.mod.tst[,c(11,13,dim(sort.reviews.mod.tst)[2])],10)

# setup as factor for confusion matrix
xgb.pred.binary = ifelse(xgb.pred.binary>.50, "Not_Helpful","Helpful")

# confusion matrix
xgb.m1.tst.cm.binary = confusionMatrix(xgb.pred.binary,
                                reviews.mod.tst$helpful.binary)
# view confusion matrix
t = xgb.m1.tst.cm.binary$table

# save model
xgb.save(xgb.binary, "xgboost.model")

# View Variable Importance
importance_matrix = xgb.importance(feature_names = ToMatch, model = xgb.binary)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix)

# ROC chart
library(pROC)

#build ROC object
rocObject = roc(reviews.mod.tst$helpful.binary, reviews.mod.tst$xgb.pred.binary)
plot(rocObject)

getwd()

library(gridExtra)
pdf("ConfusionMatrix.pdf", height=11, width=8.5)
grid.table(t)
dev.off()
