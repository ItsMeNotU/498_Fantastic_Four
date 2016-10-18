### Baseline Model to predict ratings ###
libs <- c("tm", "jsonlite","plyr","class")

lapply(libs, require, character.only = TRUE)

##load data
con  <- "/Users/tobiamartens/Desktop/Fantastic Four - Capstone/Data/reviews_Grocery_and_Gourmet_Food_5.json"

reviews <- stream_in(file(con))

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
tdm <- removeSparseTerms(tdm, 0.95)        

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

train$rating <-NULL
test$rating <- NULL

