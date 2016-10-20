### Baseline Model to predict ratings ###
libs <- c("tm", "jsonlite","plyr","class")

lapply(libs, require, character.only = TRUE)

##load data
con  <- "/Users/tobiamartens/Desktop/Fantastic Four - Capstone/Data/reviews_Grocery_and_Gourmet_Food_5.json"

reviews <- stream_in(file(con))

#create copy
reviews.copy <- reviews

#initial sample for faster processing - plan on re-running final script & model 
#on the entire dataset
sample.rows <- sample(nrow(reviews), 5000)
reviews <- reviews[sample.rows, ]

text.vector <- as.list(reviews$reviewText)
text.vector <- unlist(text.vector)
review_source <- VectorSource(text.vector)
corpus <- Corpus(review_source)


#data formatting function
clean.text <- function(x) {
        x <- tm_map(x, content_transformer(tolower))
        x <- tm_map(x, removePunctuation)
        x <- tm_map(x, stripWhitespace)
        x <- tm_map(x, removeWords, stopwords("english"))
}

corpus <- clean.text(corpus)
tdm <- TermDocumentMatrix(corpus)
tdm <- removeSparseTerms(tdm, 0.99)        

#create a list with rating as the first element and term document matrix as second
tdm <- list(Rating = reviews$overall, Tdm = tdm)

#attach rating
s.mat <- t(data.matrix(tdm[[2]]))
s.df <- as.data.frame(s.mat, stringsAsFactors = FALSE)
s.df <- cbind(s.df, rating = reviews$overall)
s.df$rating <- as.factor(s.df$rating)

#holdout sample
train.rows <- sample(nrow(s.df), ceiling(nrow(s.df)*.70))

train <- s.df[train.rows,]
test  <- s.df[-train.rows,]

train.y <- train$rating
test.y <- test$rating

#base model - SVM
#For multiclass-classification with k levels, k>2, libsvm uses the ‘one-against-one’-approach, in
#which k(k-1)/2 binary classifiers are trained; the appropriate class is found by a 
#voting scheme. degree = 3 (default)
library( 'e1071' )
svm.1 <- svm(rating~. , data = train, kernel = "polynomial", degree = 10)
svm.1.preds <- predict(svm.1, test)

#empirical error rate
err <- sum(test.y != svm.1.preds)/nrow(test)
err

#what is the error rate if we predict that every review is a 5 star
levels <- afactor(c(1,2,3,4,5))
five.stars <- factor(rep(5, nrow(test)), levels = c(1,2,3,4,5))

err.1 <- sum(test.y != five.stars)/nrow(test)
err.1
