library(jsonlite)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(tm) # Framework for text mining.
library(magrittr)
library(tidytext)
library(stringr)
library(syuzhet)
library(stringi)

###################################################
#                 Read Data                       #
###################################################

#platform
# TRUE  = "win"
# FALSE = "mac"
platform = FALSE

fName5CReviews ="reviews_Grocery_and_Gourmet_Food_5.json"
fNameRatings="ratings_Grocery_and_Gourmet_Food.csv"


wPath ="C:\\Users\\SZQ4ZD\\Documents\\Armelle\\cap\\data\\"
mPath="/Users/giovannidelgrosso/Documents/MSPA/498 - Capstone/Project/Data/"

ifelse(platform==TRUE,print("YES"),print("NO"))

if (platform) {
  path=wPath 
}  else 
{
  path=mPath
}

con  <- paste0(path,fName5CReviews)
reviews <- stream_in(file(con))
ratings = read.csv (paste0(path,fNameRatings),header=F)
colnames(ratings)<- c("reviewerID","asin","overall","unixReviewTime")

# End of - Read Data - #

# Add a review ID for convinience (no duplicates so we use the below code)
reviews$reviewID<-seq.int(nrow(reviews)) 

##########################################################################
#
# EDA
#
#########################################################################


# Missing Values in the reviews file
sapply(reviews, function(x) sum(is.na(x)))

#There are (1493) missing reviewer names.
#Check what is the reviewer ID when the name is missing
reviewerName.missing.rID <-reviews %>% filter(is.na(reviewerName)) %>%  
  group_by(reviewerID)   %>% 
  summarize (count=n())  

max(reviewerName.missing.rID$count)

reviewerName.missing.asin <-reviews %>% filter(is.na(reviewerName)) %>%  
  group_by(asin)   %>% 
  summarize (count=n()) 


reviewerName.missing.asin[which.max(reviewerName.missing.asin$count),]

#There is a product ID(B006MONQMC): that has 351 occurrencies where reviewer name is NA.
#Check this product
reviewerName.missing.asin.max<-reviewerName.missing.asin[which.max(reviewerName.missing.asin$count),]$asin
reviews %>% filter(asin==reviewerName.missing.asin.max) %>% summarise(count=n()) 
#The product appear 468 times of which 351 there is not a reviewer name

# Missing Values in the ratings file
sapply(ratings, function(x) sum(is.na(x)))

# Check for duplicated rows in review
anyDuplicated(reviews, fromLast = TRUE)
anyDuplicated(reviews[c("reviewerID","asin")], fromLast = TRUE)


# Check for duplicated rows in ratings
anyDuplicated(ratings, fromLast = TRUE)
anyDuplicated(ratings[c("reviewerID","asin")], fromLast = TRUE)

# How many unique products(8713)
reviews %>% distinct(asin) %>% summarise(n()) 

# How many unique user (14681)
reviews %>% distinct(reviewerID) %>% summarise(n())

# Explore overall ratings (column=overall)
summary(reviews [,c("overall")])
overall.summary<-reviews %>% group_by(overall) %>% summarise(count=n()) 

#Plot naming convention: pl<X>.title

pl1.reviewsByRatings<-ggplot(data=overall.summary, aes(x=overall, y=count)) +
  geom_bar(stat="identity")+
  geom_text(aes(label=count), vjust=1.6, color="white", size=3.5)+
  xlab("Ratings")+ylab("Count")+
  theme(panel.background = element_rect(fill = "lightblue", color="darkgray", size=1),
        plot.title       = element_text(family="Helvetica", lineheight=.8, face="bold.italic"))+
  ggtitle("Number of Reviews grouped by Overall Ratings")

#How does each product score
product.avgRev<- reviews %>% group_by(asin) %>% summarise(average= mean(overall))
summary(product.avgRev)

overall.byProdRating<-product.avgRev %>% group_by(average) %>% summarise(count=n()) 

pl2.reviewsByRatingsProducts<-ggplot(data=overall.byProdRating, aes(x=average, y=count)) +
  geom_line ()+
  xlab("Average Ratings")+ylab("Count")+
  theme_classic()+
  ggtitle("Number of products by Average Ratings")

# Use top and bottom rated for (could be used later for polarity analysis)
product.topRated<- product.avgRev  %>% filter(average>=4) 
product.bottomRated<- product.avgRev  %>% filter(average<=2) 

product.topRated.reviews    <-semi_join(reviews, product.topRated, by = "asin")
product.bottomRated.reviews <-semi_join(reviews, product.bottomRated, by = "asin")

#################################################################################
# source: http://juliasilge.com/blog/Ten-Thousand-Tweets/
#
#################################################################################
#Add an R Date object to reviews
reviews$timestamp <- as.Date(reviews$reviewTime,"%m %d,%Y")

# Number of Reviews over Time
pl3.numReviewsTime<-ggplot(data = reviews, aes(x = timestamp)) +
  geom_histogram(aes(fill = ..count..)) +
  theme(legend.position = "none") +
  xlab("Time") + ylab("Number of reviews") +
  scale_fill_gradient(low = "red", high = "darkred")+
  ggtitle("Number of Reviews over Time")

# Number of reviews by year (not sure if it is useful)
pl4.numReviewsByYear<-ggplot(data = reviews, aes(x = year(timestamp))) +
  geom_histogram(breaks = seq(2005, 2015, by =1), aes(fill = ..count..)) +
  theme(legend.position = "none") +
  xlab("Time") + ylab("Number of reviews") + 
  scale_fill_gradient(low = "red", high = "darkred")+
  ggtitle("Number of Reviews by Year")

# Number of reviews by week day (not sure if it is useful)
pl5.numReviewsByWeekday <- ggplot(reviews, aes(x = wday(timestamp,label=TRUE))) + 
  geom_bar(width=1, colour="gray")+
  xlab("Day of the Week") + ylab("Number of reviews") + 
  ggtitle("Number of Reviews by day of the week")


#########################################################################
#                                                                       #
#               EDA on text (reviews and summary)                       #
#                                                                       #
#########################################################################

viewDocs <- function(d,n) { d %>% extract2(n) %>% as.character() %>% writeLines()}
toString <- content_transformer(function(x, from, to) gsub(from, to, x))
toSpace<-content_transformer(function(x,pattern) gsub (pattern," ",x))
toRemove<-content_transformer(function(x,pattern) gsub (pattern,"",x))

docs <- Corpus(VectorSource(product.topRated.reviews$reviewText)) 
viewDocs(docs,2)
docs <- tm_map(docs, content_transformer(tolower))
docs<-tm_map(docs,removeNumbers)
docs<-tm_map(docs,removePunctuation)
docs<-tm_map(docs,removeWords,stopwords("english"))
docs <- tm_map(docs, removeWords, c("the","this","those","what","whether","much","isn't","also","around","furthermore","will"))
#docs<-tm_map(docs,stripWhitespace)
dictCorpus <- docs
docs<-tm_map(docs,stemDocument)
inspect(docs[1:3])
docs <- tm_map(docs, stemCompletion, dictionary=dictCorpus)
inspect(docs[1:1])

dtm <- DocumentTermMatrix(docs)
dim(dtm)
inspect(dtm[1:8, 1:2])
findFreqTerms(dtm, lowfreq=20)

myDtm <- TermDocumentMatrix(docs, control = list(minWordLength = 1))
findFreqTerms(myDtm, lowfreq=20)

findAssocs(myDtm, 'good', 0.80) # Just a test
findAssocs(myDtm, 'bad', 0.80) # Just a test
##################
# Skip this section: need to reduce dimensions to get to the wordcloud
# m <- as.matrix(myDtm)
# # calculate the frequency of words
# v <- sort(rowSums(m), decreasing=TRUE)
# myNames <- names(v)
# d <- data.frame(word=myNames, freq=v)
# rownames(d)<-NULL
# head(d, 10)
# wordcloud(d$word, d$freq, min.freq=10)
# wordCloud1.pl<- ggplot(d, aes(word, freq)) +
#   geom_bar(stat="identity") +
#   theme(axis.text.x=element_text(angle=45, hjust=1))
# 
# subset(d, d$freq>10) %>%
#   ggplot(aes(word, freq)) +
#   geom_bar(stat="identity") +
#   theme(axis.text.x=element_text(angle=45, hjust=1))
# 
# set.seed(123)
# wordcloud(d$word, d$freq, min.freq=5)
# set.seed(142)
# wordcloud(d$word, d$freq, max.words=100)
# 
# set.seed(142)
# wordcloud(d$word, d$freq, min.freq=10, colors=brewer.pal(6, "Dark2"))


#Redo the above procedure fot top rated reviews and bottom rated reviews
# Use the following data sets:
# product.topRated.reviews  
# product.bottomRated.reviews


# Not working yet
#dtm2 <- removeSparseTerms(dtm, sparse=0.3)
#inspect(dtm2[1:2, 1:2])
#kmeans5<- kmeans(dtm2, 5) #produce error


######################################################################
#                                                                    #
#Sentimenent Analysis  - Approach 1   (word Level)                   #
#citation:                                                           #
#https://www.r-bloggers.com/does-sentiment-analysis-work-a-tidy-analysis-of-yelp-reviews/
#######################################################################

rw<-reviews%>%tbl_df()
review_words <- rw %>%
  select(reviewID, reviewerID, asin, overall, reviewText) %>%
  unnest_tokens(word, reviewText) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "^[a-z']+$"))

AFINN <- sentiments %>%
  filter(lexicon == "AFINN") %>%
  select(word, afinn_score = score)

reviews_sentiment <- review_words %>%
  inner_join(AFINN, by = "word") %>%
  group_by(reviewID,overall) %>%
  summarise(sentiment = mean(afinn_score))

ggplot(reviews_sentiment, aes(overall, sentiment, group = overall)) +
  geom_boxplot() +
  ylab("Average sentiment score")+theme_bw()

review_words_counted <- review_words %>%
  count(reviewID,reviewerID,asin, overall, word) %>%
  ungroup()

word_summaries <- review_words_counted %>%
  group_by(word) %>%
  summarize(products = n_distinct(asin),
            reviews = n(),
            uses = sum(n),
            average_stars = mean(overall)) %>%
  ungroup()

word_summaries_filtered <- word_summaries %>%
  filter(reviews >= 600, products >= 20)

#Most positive and negative words
word_summaries_filtered %>%arrange(desc(average_stars))

word_summaries_filtered %>% arrange(average_stars)
ggplot(word_summaries_filtered, aes(reviews, average_stars)) +
  geom_point() +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1, hjust = 1) +
  scale_x_log10() +
  geom_hline(yintercept = mean(reviews$overall), color = "red", lty = 2) +
  xlab("# of reviews") +
  ylab("Average Stars")

words_afinn <- word_summaries_filtered %>%
  inner_join(AFINN)

ggplot(words_afinn, aes(afinn_score, average_stars, group = afinn_score)) +
  geom_boxplot() +
  xlab("AFINN score of word") +
  ylab("Average stars of reviews with this word")


################################################
#
# Sentiment Part 2  (review level)          
#
################################################

review.sentiment.reviewText   <- get_nrc_sentiment(reviews$reviewText)
review.sentiment.summaryText  <- get_nrc_sentiment(reviews$summary)
colnames(review.sentiment.summaryText) <- paste("ST", colnames(review.sentiment.summaryText), sep = "_")
reviews2 <- cbind(reviews, review.sentiment.reviewText,review.sentiment.summaryText)

sentimentTotals.reviewText <- data.frame(colSums(reviews2[,c(11:18)]))
names(sentimentTotals.reviewText) <- "count"
sentimentTotals.reviewText <- cbind("sentiment" = rownames(sentimentTotals.reviewText), sentimentTotals.reviewText)
rownames(sentimentTotals.reviewText) <- NULL
ggplot(data = sentimentTotals.reviewText, aes(x = sentiment, y = count)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Total Count") + ggtitle("Total Sentiment Score for All Reviews")

review.sentiment.summaryText <- data.frame(colSums(reviews2[,c(21:28)]))
names(review.sentiment.summaryText) <- "count"
review.sentiment.summaryText <- cbind("sentiment" = rownames(review.sentiment.summaryText), review.sentiment.summaryText)
rownames(review.sentiment.summaryText) <- NULL
ggplot(data = review.sentiment.summaryText, aes(x = sentiment, y = count)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Total Count") + ggtitle("Total Sentiment Score for All Summary Review")

reviews2$posneg<-reviews2$positive-reviews2$negative
reviews2$ST_posneg<-reviews2$ST_positive-reviews2$ST_negative

ggplot(reviews2, aes(x=posneg, y=overall)) +
  geom_point(shape=1) +
  ggtitle("Positive/Negative vs. Overall")

reviews2$reviewDepth<- stri_count(reviews$reviewText,regex="\\S+") 

reviews2$helpful<- gsub("[,|:]", "-", reviews2$helpful, perl = T)
reviews2$helpful<- gsub("[^\\d | ^-]", "", reviews2$helpful, perl = T)
reviews2$helpful<- gsub("[[:space:]]", "", reviews2$helpful,perl = T)
reviews2<-reviews2 %>% separate(helpful, into = c("H1","H2"),remove=FALSE,convert=TRUE)

rev.sub<- reviews2 %>%
  mutate (target = ifelse(H2==0,"NA", round((H1/H2),4))) %>%
  
  ggplot(dat, aes(x=xvar, y=yvar, color=cond)) + geom_point(shape=1)
ggplot(compare.feedback, aes(x=reviewID, y=overall)) +
  geom_point(shape=1) +
  ggtitle("Positive/Negative vs. Overall")

#######################################
#     tf-idf - Manual Approach        #
#######################################
#Total number of reviews
reviews.total= reviews%>% distinct(reviewID) %>% summarise(n()) 

review_words_by_review <- review_words %>% group_by(reviewID) %>%
  mutate(n.wordsInOneReview = n()) %>% 
  ungroup()%>%
  group_by(reviewID,word) %>%
  mutate(n.wordInOneReview = n()) %>% 
  ungroup()%>%
  group_by(word)%>% distinct(reviewID) %>% 
  mutate(n.wordInReviews=n())  %>% ungroup()

v=as.numeric(rep(reviews.total,nrow(review_words_by_review)))/review_words_by_review$n.wordInReviews
review_words_by_review$tf = round((review_words_by_review$n.wordInOneReview/review_words_by_review$n.wordsInOneReview),4)
review_words_by_review$idf = round(log(v),4)
review_words_by_review$tf_idf = round(review_words_by_review$tf*review_words_by_review$idf,4)  

word.relevance<-review_words_by_review %>%
  select (overall,word,tf_idf) %>%
  distinct(word) %>%
  arrange(desc(tf_idf))

ggplot(word.relevance %>% head(60), aes(word,tf_idf, fill = overall))+coord_flip()+
  geom_bar(stat = "identity") +
  labs(title = "Highest tf-idf words in Reviews",
       x = NULL, y = "tf-idf") +  scale_colour_brewer(palette = "Paired")

ggplot(word.relevance %>% filter(tf_idf<=3) %>% head(60), aes(word,tf_idf, fill = overall))+coord_flip()+
  geom_bar(stat = "identity") +
  labs(title = "Lowest tf-idf words in Reviews",
       x = NULL, y = "tf-idf")

review_words_by_review %>% filter( tf_idf>5) %>%
  ggplot(aes(word, tf_idf)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ overall, scales = "free") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab("TF-IDF")

######################################
#   tf-idf - Alternative Approach    #
# Using a package                    #
######################################

head(reviews)
reviews$reviewText <- toLower(reviews$reviewText)
review_words <- reviews %>%
  select(reviewID, reviewerID,asin,overall, reviewText) %>%
  unnest_tokens(word, reviewText) %>%
  filter(!word %in% stop_words$word,str_detect(word, "^[a-z']+$")) %>%
  count(reviewID, reviewerID,asin,overall, word, sort = TRUE) %>%
  ungroup()
review_words<-review_words %>%
  bind_tf_idf(word, reviewID, n) %>%
  arrange(desc(tf_idf))

# the results may not be accurate need to be reviewed in depth.
# in some cases the tf column has the same value as the count column 
# reviews %>%filter(reviewID==94391)

# Reposition this plot and set.
# review_word_counts %>%
#   head(30) %>%
#   mutate(Word = reorder(Word, n)) %>%
#   ggplot(aes(Word, n)) +
#   geom_bar(stat = "identity") +
#   ylab("Number of appearances in reviews") +
#   coord_flip()

###################################################################
# EDA Based Modeling
#
###################################################################

# We start with the data set that contains sentiments and polarity
# Data set is: rev.sub (need to remove NA from target col)
# Need also to selected a limited number of features.
#