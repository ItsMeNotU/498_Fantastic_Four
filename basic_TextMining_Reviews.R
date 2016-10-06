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

#split original file into 5 subsets, each for one star
one_star <- reviews[reviews$overall==1,]
two_star <- reviews[reviews$overall==2,]
three_star <- reviews[reviews$overall==3,]
four_star <- reviews[reviews$overall==4,]
five_star <-reviews[reviews$overall==5,]

most.frequent.terms <- function(x){
        review.text <- paste(x, collapse = " ")
        review_source <- VectorSource(review.text)
        corpus <- Corpus(review_source)
        corpus <- tm_map(corpus, content_transformer(tolower))
        corpus <- tm_map(corpus, removePunctuation)
        corpus <- tm_map(corpus, stripWhitespace)
        corpus <- tm_map(corpus, removeWords, stopwords("english"))
        dtm <- DocumentTermMatrix(corpus)
        dtm2 <- as.matrix(dtm)
        frequency <- colSums(dtm2)
        frequency <- sort(frequency, decreasing = TRUE)
        frequency
}
        
one.star.terms <- most.frequent.terms(one_star$reviewText)
two.star.terms <- most.frequent.terms(two_star$reviewText)
three.star.terms <- most.frequent.terms(three_star$reviewText)
four.star.terms <- most.frequent.terms(four_star$reviewText)
five.star.terms <- most.frequent.terms(five_star$reviewText)

#wordcloud for one.star.terms
words <- names(one.star.terms)
wordcloud(words[1:100], one.star.terms[1:100])

#wordcloud for two.star.terms
words <- names(two.star.terms)
wordcloud(words[1:100], two.star.terms[1:100])

#wordcloud for three.star.terms
words <- names(three.star.terms)
wordcloud(words[1:100], three.star.terms[1:100])

#wordcloud for four.star.terms
words <- names(four.star.terms)
wordcloud(words[1:100], four.star.terms[1:100])

#wordcloud for five.star.terms
words <- names(five.star.terms)
wordcloud(words[1:100], five.star.terms[1:100])
