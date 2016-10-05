#load package for text mining
library(Rcampdf)

library(tm)

library(jsonlite)

#load data
con  <- "/Users/tobiamartens/Desktop/Fantastic Four - Capstone/Data/reviews_Grocery_and_Gourmet_Food_5.json"

reviews <- stream_in(file(con))

review.text <- paste(reviews$reviewText, collapse = " ")

#setting up source & vector
review_source <- VectorSource(review.text)
corpus <- Corpus(review_source)

#cleaning
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removeWords, stopwords("english"))

#create document matrix
dtm <- DocumentTermMatrix(corpus)
dtm2 <- as.matrix(dtm)

#finding the most frequent terms
frequency <- colSums(dtm2)
frequency <- sort(frequency, decreasing = TRUE)
head(frequency)

#quick word cloud of the most frequent words
library('wordcloud')
words <- names(frequency)
wordcloud(words[1:100], frequency[1:100])
