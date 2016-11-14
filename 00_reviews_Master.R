#==============================================================================
#==============================================================================
# 00_reviews_Master
# Last Updated: 2016-11-12 by MJG
#==============================================================================
#==============================================================================

# Clear workspace
rm(list=ls())

# Load libraries
library(caret)
library(doParallel)
library(dplyr)
library(ggplot2)
library(httr)
library(jsonlite)
library(randomForest)
library(stringr)
library(syuzhet)
library(tidytext)
library(tm)
library(wordcloud)

#==============================================================================
# S00 | Table of Contents
#==============================================================================
# To quickly get to a section search for "S{X}" where {X} corresponds below:
#   S00 - Table of Contents
#   S01 - Functions
#   S02 - API Call
#   S03 - Data Import, Quality Check, and Prep
#   S04 - EDA
#   S05 - Text Analysis
#   S06 - Sentiment Analysis
#   S07 - TF-IDF
#   S08 - Model Prep
#   S09 - Model Build (Classification)
#   S10 - Model Build (Regression)

#==============================================================================
# S01 | Functions
#==============================================================================

#------------------------------------------------------------------------------
# External Functions
#------------------------------------------------------------------------------
# Source functions from GitHub
source.GitHub = function(url){
    require(RCurl)
    sapply(url, function(x){
        eval(parse(text = getURL(x, 
                                 followlocation = T,
                                 cainfo = system.file("CurlSSL",
                                                      "cacert.pem",
                                                      package = "RCurl"))),
             envir = .GlobalEnv)
    })
}

# Assign URL and source functions
url = "http://bit.ly/1T6LhBJ"
source.GitHub(url); rm(url)

#------------------------------------------------------------------------------
# Text Cleaning
#------------------------------------------------------------------------------
text.clean = function(df, stop.words, sparse, freq = FALSE){
    require(tm)
    # Set valid range for sparse and check
    if (!missing(sparse)){
        val.range = round(seq(from = 0.01, to = 0.99, by = 0.01), digits = 2)
        sparse = round(sparse, digits = 2)
        if (!sparse %in% val.range){
            stop("The value of sparse must be between 0.01 and 0.99.")
        }
    }
    # Check for sparse value and freq = TRUE
    if (!missing(sparse) && freq){
        warning("Frequency argument ignored when sparse value given.")
    }
    # Create corpus
    if (!freq){
        temp = Corpus(VectorSource(df))
    }
    if (freq){
        temp = Corpus(VectorSource(paste(df, collapse = " ")))
    }
    # Basic cleaning functions
    temp = tm_map(temp, content_transformer(tolower))
    temp = tm_map(temp, removeNumbers)
    temp = tm_map(temp, removePunctuation)
    temp = tm_map(temp, stripWhitespace)
      # Check for additional stop words
      if (!missing(stop.words)){
          temp = tm_map(temp, removeWords, c(stopwords("english"), stop.words))
      } else {
          temp = tm_map(temp, removeWords, stopwords("english"))
      }
    temp = tm_map(temp, stemDocument)
    # Check for sparse argument
    if (!missing(sparse)){
        tdm = TermDocumentMatrix(temp)
        tdm = removeSparseTerms(tdm, sparse = sparse)
        return(tdm)
    }
    # Check for frequency argument
    if (missing(sparse)){
        if (!freq){
            return(temp)
        }
        if (freq){
            dtm = as.matrix(DocumentTermMatrix(temp))
            freq = colSums(dtm)
            freq = sort(freq, decreasing = TRUE)
            head(freq, n = 100)
        }
    }
}

#==============================================================================
# S02 - API Call
#==============================================================================

# Specify url and path
url = "http://search-predict-498-etrnmkcw2ss5k2tne664bemmjq.us-west-2.es.amazonaws.com/"
path = "reviews/_search?q=coffee"

# Get raw result
raw.result = GET(url = url, path = path)

# Convert raw result content to character string
raw.content = rawToChar(raw.result$content)

# Download data and convert from JSON
api.content = fromJSON(raw.content)

#==============================================================================
# S03 - Data Import, Quality Check, and Prep
#==============================================================================

#------------------------------------------------------------------------------
# Import
#------------------------------------------------------------------------------
# Load data
reviews = stream_in(file("reviews_Grocery_and_Gourmet_Food_5.json.gz"))

# View structure
str(reviews)

#------------------------------------------------------------------------------
# Quality Check
#------------------------------------------------------------------------------
# Check for duplicated rows
anyDuplicated(reviews,
              fromLast = TRUE)
anyDuplicated(reviews[c("reviewerID", "asin")],
              fromLast = TRUE)

# Add a primary key to use for convenience
reviews$reviewsPK = seq.int(nrow(reviews))

# Reorder to put reviewsPK first
reviews = reviews[c(10, 1:9)]

#--------------------------------------
# Missing Values
#--------------------------------------
# Check for NA values
colSums(is.na(reviews))[colSums(is.na(reviews)) > 0]

# Examine reviewerID when reviewerName is missing
miss.reviewerName.reviewerID = reviews %>%
                                   filter(is.na(reviewerName)) %>%
                                   group_by(reviewerID) %>%
                                   summarize(count = n())

# reviewerID of max times reviewerName is missing
miss.reviewerName.reviewerID[which.max(miss.reviewerName.reviewerID$count), ]

# Examine asin when reviewerName is missing
miss.reviewerName.asin = reviews %>%
                             filter(is.na(reviewerName)) %>%
                             group_by(asin) %>%
                             summarize(count = n())

# asin of max times reviewerName is missing
miss.reviewerName.asin[which.max(miss.reviewerName.asin$count), ]

# asin of B006MONQMC shows reviewerName is missing 351 times, examine further
# asin of B006MONQMC has 468 reviews = name missing 75.00% of the time
miss.reviewerName.asin.max = 
    miss.reviewerName.asin[which.max(miss.reviewerName.asin$count), ]$asin
reviews %>%
    filter(asin == miss.reviewerName.asin.max) %>% 
    summarize(count = n())

# Replace NA values in reviewerName with blank
#   Note: necessary for modeling on test data
reviews$reviewerName[is.na(reviews$reviewerName)] = ""

# Tidy workspace
rm(list=ls(pattern = "^miss"))

#------------------------------------------------------------------------------
# Prep
#------------------------------------------------------------------------------
# Rename existing variables
names(reviews)[names(reviews)=="overall"] = "overall.num"

# Create flags, percentage score, and bins for reviews$helpful
reviews$helpful.up = sapply(reviews$helpful, function(x) x[1])
reviews$helpful.total = sapply(reviews$helpful, function(x) x[2])
reviews$helpful.nan = sapply(reviews$helpful,
                             function(x) ifelse(x[2] == 0, 1, 
                                             ifelse(x[1]>x[2], 1, 0)))
reviews$helpful.perc = sapply(reviews$helpful,
                              function(x) ifelse(x[2] == 0, NA, 
                                              ifelse(x[1]>x[2], NA, x[1]/x[2])))
reviews$helpful.bins = cut(reviews$helpful.perc,
                           breaks = 3,
                           include.lowest = TRUE,
                           labels = c("Lower", "Middle", "Upper"))

# Convert existing variables
reviews$overall.fac = as.factor(reviews$overall.num)
reviews$reviewerID = as.factor(reviews$reviewerID)
reviews$asin = as.factor(reviews$asin)

# Create additional variables
reviews$reviewText.count = sapply(gregexpr("\\S+",
                                           reviews$reviewText),
                                  length)
reviews$summary.count = sapply(gregexpr("\\S+",
                                        reviews$summary),
                               length)
reviews$time.stamp = as.Date(as.POSIXct(reviews$unixReviewTime,
                                        origin="1970-01-01"))
reviews$time.weekday = as.factor(weekdays(reviews$time.stamp))
reviews$time.months = as.factor(months(reviews$time.stamp))
reviews$time.year = as.factor(substr(reviews$time.stamp, 1, 4))
reviews$time.min = ave(reviews$time.stamp,
                       reviews$asin,
                       FUN = min)

# Drop helpful
reviews = subset(reviews, select = -c(helpful))

#==============================================================================
# S04 - EDA
#==============================================================================

# Subset reviews for EDA
reviews.eda = reviews[reviews$helpful.nan == 0, ]

#------------------------------------------------------------------------------
# Traditional - Quantitative
#------------------------------------------------------------------------------
# Frequency of reviewerID
freq.reviewerID = t(fac.freq(reviews.eda$reviewerID,
                             cat = FALSE))

# Number of unique reviewerID
nlevels(reviews.eda$reviewerID)

# Frequency of asin
freq.asin = t(fac.freq(reviews.eda$asin,
                       cat = FALSE))

# Number of unique asin
nlevels(reviews.eda$asin)

#--------------------------------------
# Variable: helpful.bins
#--------------------------------------
# Frequency of helpful bins
freq.helpful = fac.freq(reviews.eda$helpful.bins,
                        cat = FALSE)

# Frequency of helpful bins by reviewerID
freq.helpful.reviewerID = fac.freq(reviews.eda$reviewerID, 
                                   reviews.eda$helpful.bins)

# Frequency of helpful bins by asin
freq.helpful.asin = fac.freq(reviews.eda$asin,
                             reviews.eda$helpful.bins)

# Frequency of helpful bins by year
freq.year = fac.freq(reviews.eda$time.year,
                     reviews.eda$helpful.bins)

# Count of upper helpful bin by asin
count.helpful.up = reviews.eda %>%
                       group_by(asin) %>%
                       filter(helpful.bins == "Upper") %>%
                       count(helpful.bins, 
                             sort = TRUE)

#--------------------------------------
# Variable: overall.num
#--------------------------------------
# Summary statistics on overall.num
summary(reviews.eda$overall.num)

# Mean of overall.num by asin
mean.overall.asin = reviews.eda %>%
                        group_by(asin) %>%
                        summarise(average = mean(overall.num))

# Store top and bottom values of mean overall.num by asin
#    Could be used later for polarity analysis
mean.overall.asin.top = mean.overall.asin %>%
                            filter(average >= 4) 
mean.overall.asin.bot = mean.overall.asin %>%
                            filter(average <= 2) 

# Create subset of reviews containing top and bottom reviews by overall.num
reviews.eda.overall.top = semi_join(reviews.eda,
                                    mean.overall.asin.top,
                                    by = "asin")
reviews.eda.overall.bot = semi_join(reviews.eda, 
                                    mean.overall.asin.bot,
                                    by = "asin")

# Count of rounded mean for overall.num by asin
count.overall.asin = mean.overall.asin %>%
                         group_by(average = round(average/0.125)*0.125) %>%
                         summarize(count = n())

#--------------------------------------
# Variable: overall.fac
#--------------------------------------
# Frequency of overall.fac
freq.overall = fac.freq(reviews.eda$overall.fac,
                        cat = FALSE)

# Frequency of overall.fac by reviewerID
freq.overall.reviewerID = fac.freq(reviews.eda$reviewerID,
                                   reviews.eda$overall.fac)

# Frequency of overall.fac by asin
freq.overall.asin = fac.freq(reviews.eda$asin,
                             reviews.eda$overall.fac)

# Frequency of overall.fac by year
freq.overall.year = fac.freq(reviews.eda$time.year,
                             reviews.eda$overall.fac)

# Count of overall.fac by asin for 5-star reviews
count.overall.fs = reviews.eda %>%
                       group_by(asin) %>%
                       filter(overall.fac == "5") %>%
                       count(overall.fac, 
                             sort = TRUE)

#------------------------------------------------------------------------------
# Traditional - Qualitative
#------------------------------------------------------------------------------
# Barplot of overall ratings
ggplot(data = reviews.eda,
       aes(x = overall.fac)) +
    geom_bar(fill = "grey50") +
    stat_count(aes(label = ..count..),
               vjust = 1.6,
               color = "white",
               size = 3.5,
               geom = "text") +
    labs(title = "Barplot of Overall Product Ratings",
         x = "Overall Rating",
         y = "Count")

# Barplot of overall ratings by year
ggplot(data = reviews.eda,
       aes(x = overall.fac)) +
    geom_bar(fill = "grey50") +
    facet_wrap(~time.year) +
    labs(title = "Barplot of Overall Product Ratings",
         x = "Overall Rating",
         y = "Count")

# Barplot of rounded mean of overall.num by asin (count)
ggplot(data = count.overall.asin,
       aes(x = average,
           y = count)) +
    geom_bar(stat = "identity") +
    labs(title = "Number of Products by Mean Overall Rating",
         x = "Mean Overall Rating\n(rounded to nearest 1/8)",
         y = "Count")

# Barplot of helpful bins
ggplot(data = reviews.eda,
       aes(x = helpful.bins)) +
    geom_bar(fill = "grey50") +
    stat_count(aes(label = ..count..),
               vjust = 1.6,
               color = "white",
               size = 3.5,
               geom = "text") +
    labs(title = "Barplot of Product Ratings by Helpful Bins",
         x = "Helpful Bins",
         y = "Count")

# Barplot of helpful bins by year
ggplot(data = reviews.eda,
       aes(x = helpful.bins)) +
    geom_bar(fill = "grey50") +
    facet_wrap(~time.year) +
    labs(title = "Barplot of Product Ratings by Helpful Bins",
         x = "Helpful Bins",
         y = "Count")

# Count of reviews over time
ggplot(data = reviews.eda %>%
                  count(time.stamp,
                        sort = FALSE),
       aes(x = time.stamp,
           y = n)) +
    geom_line() +
    labs(title = "Number of Product Reviews Over Time",
         x = "Time",
         y = "Number of Product Reviews")

#==============================================================================
# S05 - Text Analysis
#==============================================================================

#------------------------------------------------------------------------------
# Word Counts (Mean)
#------------------------------------------------------------------------------

#--------------------------------------
# Variable: reviewText.count
#--------------------------------------
# Mean word count for review text by helpful bins
mean.reviewText.lo = round(mean(reviews.eda$reviewText.count
                                [reviews.eda$helpful.bins == "Lower"]),
                           0)
mean.reviewText.md = round(mean(reviews.eda$reviewText.count
                                [reviews.eda$helpful.bins == "Middle"]),
                           0)
mean.reviewText.up = round(mean(reviews.eda$reviewText.count
                                [reviews.eda$helpful.bins == "Upper"]),
                           0)
count.reviewText = data.frame(Levels = levels(reviews.eda$helpful.bins),
                              Count = c(mean.reviewText.lo,
                                        mean.reviewText.md,
                                        mean.reviewText.up))

#--------------------------------------
# Variable: summary.count
#--------------------------------------
# Mean word count for summary by helpful bins
mean.summary.lo = round(mean(reviews.eda$summary.count
                             [reviews.eda$helpful.bins == "Lower"]),
                        0)
mean.summary.md = round(mean(reviews.eda$summary.count
                             [reviews.eda$helpful.bins == "Middle"]),
                        0)
mean.summary.up = round(mean(reviews.eda$summary.count
                             [reviews.eda$helpful.bins == "Upper"]),
                        0)
count.summary = data.frame(Levels = levels(reviews.eda$helpful.bins),
                           Count = c(mean.summary.lo,
                                     mean.summary.md,
                                     mean.summary.up))

#------------------------------------------------------------------------------
# Frequent Terms (Top 100)
#------------------------------------------------------------------------------
# All helpful bins
mft = text.clean(reviews.eda$reviewText,
                 freq = TRUE)

# Lower helpful bin
mft.lo = text.clean(reviews.eda$reviewText
                    [reviews.eda$helpful.bins == "Lower"],
                    freq = TRUE)

# Middle helpful bin
mft.md = text.clean(reviews.eda$reviewText
                    [reviews.eda$helpful.bins == "Middle"],
                    freq = TRUE)

# Upper helpful bin
mft.up = text.clean(reviews.eda$reviewText
                    [reviews.eda$helpful.bins == "Upper"],
                    freq = TRUE)

#------------------------------------------------------------------------------
# Word Clouds
#------------------------------------------------------------------------------
# Overall
wordcloud(names(mft),
          mft)

# Lower helpful bin
wordcloud(names(mft.lo),
          mft.lo)

# Middle helpful bin
wordcloud(names(mft.md),
          mft.md)

# Upper helpful bin
wordcloud(names(mft.up),
          mft.up)

#------------------------------------------------------------------------------
# Stop Words
#------------------------------------------------------------------------------
# Create stop list based on frequencies and word clouds
stop.list = c("also",
              "around",
              "can",
              "coffe",
              "flavor",
              "furthermore",
              "good",
              "isn",
              "just",
              "like",
              "much",
              "one",
              "product",
              "tast",
              "tea",
              "the",
              "this",
              "those",
              "what",
              "whether",
              "will")

#==============================================================================
# S06 - Sentiment Analysis
#==============================================================================
# More info: http://bit.ly/2apeTZl

#------------------------------------------------------------------------------
# Word Level
#------------------------------------------------------------------------------
# Create tidy dataset (long) of key words by reviewsPK
words = reviews.eda %>%
            tbl_df() %>%
            select(reviewsPK,
                   reviewerID,
                   asin,
                   overall.num,
                   reviewText) %>%
            unnest_tokens(word,
                          reviewText) %>%
            filter(!word %in% stop_words$word,
                   str_detect(word, "^[a-z']+$"))

# Score using AFINN lexicon, scores range from {-5, 5}
AFINN = sentiments %>%
            filter(lexicon == "AFINN") %>%
            select(word, 
                   afinn.score = score)

# Now join scores back to words
words.afinn = words %>%
                  inner_join(AFINN,
                             by = "word") %>%
                  group_by(reviewsPK,
                           overall.num) %>%
                  summarize(afinn.score.mean = mean(afinn.score))

# Collapse and add count of word frequency in each review
words.count = words %>%
                  count(reviewsPK,
                        reviewerID,
                        asin,
                        overall.num,
                        word) %>%
                  ungroup()

# Summary statistics on unique words in reviews
words.stats = words.count %>%
                  group_by(word) %>%
                  summarize(products = n_distinct(asin),
                            reviews = n(),
                            uses = sum(n),
                            average.overall = mean(overall.num)) %>%
                  ungroup()

# Create subset of summary statistics
words.stats.sub = words.stats %>%
                      filter(reviews >= 600,
                             products >= 20)

# Explore most positive words of subset
words.stats.sub %>% 
    arrange(desc(average.overall))

# Explore most negative words of subset
words.stats.sub %>% 
    arrange(average.overall)

# Score subset using AFINN lexicon, scores range from {-5, 5}
#   Note: Very few observations, looks like some words do not exist in AFINN,
#   (e.g. absolutely) and therefore are not represented here.
words.stats.sub.afinn = words.stats.sub %>% 
                            inner_join(AFINN)

#--------------------------------------
# Plots
#--------------------------------------
# Scatterplot of review words by quantity and overall rating
ggplot(data = words.stats.sub,
       aes(x = reviews, 
           y = average.overall)) +
    geom_point() +
    geom_text(aes(label = word), 
              check_overlap = TRUE, 
              vjust = 1, 
              hjust = 1) +
    scale_x_log10() +
    geom_hline(yintercept = mean(reviews$overall.num), 
               color = "red", 
               lty = 2) +
    labs(title = "Review Keywords by Quantity and Overall Product Rating",
         x = "Number of Reviews",
         y = "Overall Rating")

# Boxplot of AFINN score by overall.num
ggplot(data = words.afinn, 
       aes(x = overall.num, 
           y = afinn.score.mean, 
           group = overall.num)) +
    geom_boxplot() +
    labs(title = "Mean AFINN Score by Overall Product Rating",
         x = "Overall Rating",
         y = "Mean Sentiment Score")

# Boxplot of mean overall ratings of reviews with words by AFINN score
ggplot(data = words.stats.sub.afinn,
       aes(x = afinn.score,
           y = average.overall,
           group = afinn.score)) +
    geom_boxplot() +
    labs(title = "AFINN Score of Keywords by Mean Overall Rating of Review",
         x = "AFINN Score of Keyword",
         y = "Mean Overall Rating of Review with Keyword")

#------------------------------------------------------------------------------
# Review Level
#------------------------------------------------------------------------------

#--------------------------------------
# Variable: reviewText
#--------------------------------------
# Conduct sentiment analysis on reviewText
sent.reviewText = get_nrc_sentiment(reviews.eda$reviewText)

# Create dataset with sentiment totals
sent.reviewText.tot = data.frame(colSums(sent.reviewText))
names(sent.reviewText.tot) = "count"
sent.reviewText.tot = cbind("sentiment" = rownames(sent.reviewText.tot),
                            sent.reviewText.tot)
rownames(sent.reviewText.tot) = NULL

#--------------------------------------
# Variable: summary
#--------------------------------------
# Conduct sentiment analysis on summary
sent.summaryText = get_nrc_sentiment(reviews.eda$summary)

# Create dataset with sentiment totals
sent.summaryText.tot = data.frame(colSums(sent.summaryText))
names(sent.summaryText.tot) = "count"
sent.summaryText.tot = cbind("sentiment" = rownames(sent.summaryText.tot),
                             sent.summaryText.tot)
rownames(sent.summaryText.tot) = NULL

#--------------------------------------
# Plots
#--------------------------------------
# Total sentiment score based on review text by adjective
ggplot(data = sent.reviewText.tot,
       aes(x = sentiment,
           y = count)) +
    geom_bar(aes(fill = sentiment),
             stat = "identity") +
    theme(legend.position = "none") +
    labs(title = "Total Sentiment Score for Review Text",
         x = "Sentiment - Review Text",
         y = "Total Count")

# Total sentiment score based on summary text by adjective
ggplot(data = sent.summaryText.tot,
       aes(x = sentiment,
           y = count)) +
    geom_bar(aes(fill = sentiment), stat = "identity") +
    theme(legend.position = "none") +
    labs(title = "Total Sentiment Score for Summary Review Text",
         x = "Sentiment - Summary Review Text",
         y = "Total Count")

#------------------------------------------------------------------------------
# Sentiment dataset
#------------------------------------------------------------------------------
# Add prefix to reviewText variables before joining
colnames(sent.reviewText) = paste("RT",
                                  colnames(sent.reviewText),
                                  sep = "_")

# Add prefix to summaryText variables before joining
colnames(sent.summaryText) = paste("ST",
                                   colnames(sent.summaryText),
                                   sep = "_")

# Create clone of reviews.eda and join sentiment variables
reviews.sent = cbind(reviews.eda,
                     sent.reviewText,
                     sent.summaryText)

# Create additional variables
reviews.sent$RT_posneg = reviews.sent$RT_positive - reviews.sent$RT_negative
reviews.sent$ST_posneg = reviews.sent$ST_positive - reviews.sent$ST_negative

#--------------------------------------
# Plots
#--------------------------------------
# Boxplot of sentiment score of review text by overall rating
ggplot(data = reviews.sent,
       aes(x = overall.num,
           y = RT_posneg,
           group = overall.num)) +
    geom_boxplot() +
    labs(title = "Sentiment of Review Text by Overall Rating",
         x = "Overall Rating",
         y = "Positive Less Negative Score")

# Boxplot of sentiment score of summary text by overall rating
ggplot(data = reviews.sent,
       aes(x = overall.num,
           y = ST_posneg,
           group = overall.num)) +
    geom_boxplot() +
    labs(title = "Sentiment of Summary Review Text by Overall Rating",
         x = "Overall Rating",
         y = "Positive Less Negative Score")

#==============================================================================
# S07 - TF-IDF
#==============================================================================

# Create version of reviews for TF-IDF analysis
reviews.tfidf = reviews.sent

# Isolate words in given review and count word frequency across all reviews
tfidf = words %>% 
            group_by(reviewsPK) %>%
            mutate(word.count = n()) %>% 
            ungroup() %>%
            group_by(reviewsPK,
                     word) %>%
            mutate(word.freq.one = n()) %>% 
            ungroup() %>%
            group_by(word) %>% 
            distinct(reviewsPK, 
                     .keep_all = TRUE) %>% 
            mutate(word.freq.all = n()) %>% 
            ungroup()

# Calculate TF, IDF, TF-IDF
tfidf$tf = round(tfidf$word.freq.one / tfidf$word.count,
                 digits = 4)
tfidf$idf = round(log1p(nrow(unique(reviews.tfidf)) / tfidf$word.freq.all),
                  digits = 4)
tfidf$tfidf = round(tfidf$tf * tfidf$idf,
                    digits = 4)

# Group and arrange by TF-IDF score
tfidf.relevance = tfidf %>%
                      select(overall.num,
                             word,
                             tfidf) %>%
                      distinct(word,
                               .keep_all = TRUE) %>%
                      arrange(desc(tfidf))

#--------------------------------------
# Plots
#--------------------------------------
# Top 60 TF-IDF words in reviews
ggplot(data = head(tfidf.relevance,
                   n = 60),
       aes(word,
           tfidf,
           fill = overall.num)) +
    coord_flip() +
    geom_bar(stat = "identity") +
    labs(title = "Top 60 TF-IDF Words in Reviews",
         x = NULL,
         y = "TF-IDF") +
    scale_colour_brewer(palette = "Paired")

# Bottom 60 TF-IDF words in reviews
#   Plot 1
ggplot(data = head(tfidf.relevance[tfidf.relevance$tfidf <= 3, ],
                   n = 60),
       aes(word,
           tfidf,
           fill = overall.num)) +
    coord_flip() +
    geom_bar(stat = "identity") +
    labs(title = "Bottom 60 TF-IDF Words in Reviews",
         x = NULL,
         y = "TF-IDF") +
    scale_colour_brewer(palette = "Paired")

# Top 60 TF-IDF words in reviews
#   Plot 2
ggplot(data = tail(tfidf.relevance,
                   n = 60),
       aes(word,
           tfidf,
           fill = overall.num)) +
    coord_flip() +
    geom_bar(stat = "identity") +
    labs(title = "Bottom 60 TF-IDF Words in Reviews",
         x = NULL,
         y = "TF-IDF") +
    scale_colour_brewer(palette = "Paired")

# Top TF-IDF words by overall rating
ggplot(data = tfidf[tfidf$tfidf > 5, ],
       aes(word,
           tfidf)) +
    geom_bar(stat = "identity") +
    facet_wrap(~overall.num,
               scales = "free") +
    theme(axis.text.x = element_text(angle = 90,
                                     hjust = 1)) +
    labs(title = "Top TF-IDF Words by Overall Rating",
         x = "Word\n(Only TF-IDF Score > 5 Shown)",
         y = "TF-IDF")

#------------------------------------------------------------------------------
# TF-IDF dataset
#------------------------------------------------------------------------------
# Create temp matrix to later join words and counts
#   Note: use top 200 TF-IDF words
temp = matrix(nrow = nrow(reviews.sent),
              ncol = nrow(tfidf.relevance[1:200, ]))
colnames(temp) = tfidf.relevance$word[1:200]

# Join
reviews.tfidf = cbind(reviews.sent, temp)

# Verify no NA values before replacing
colSums(is.na(reviews.sent))[colSums(is.na(reviews.sent)) > 0]

# Replace NA values with zero
reviews.tfidf[is.na(reviews.tfidf)] = 0

# Join counts
for (review in reviews.tfidf$reviewsPK) {
    idx = tfidf$reviewsPK == review
    words = tfidf$word[idx]
    idx.top = words %in% tfidf.relevance.top$word
    counts = tfidf$word.freq.one[idx]
    reviews.tfidf[which(reviews.tfidf$reviewsPK == review),
                  words[idx.top]] = counts[idx.top]
}

# Clean-up
rm(temp)

#==============================================================================
# S08 - Model Prep
#==============================================================================

# Create version of reviews for modeling
reviews.mod = reviews.tfidf

# Clean-up workspace
rm.list = "^AFINN|^count|^freq|^mean|^mft|^reviews.eda.|^sent|^words"
rm(list = ls(pattern = paste(rm.list))); rm(rm.list)

#------------------------------------------------------------------------------
# Train-Test Split
#------------------------------------------------------------------------------
# Create train index
set.seed(123)
trn.idx = createDataPartition(reviews.mod$helpful.bins,
                              p = 0.70,
                              list = F)

# Validate split; target = 0.70
length(trn.idx) / nrow(reviews.mod)

# Create test index
#   Note: This is a little tricky because row names are not consecutive, since
#   "reviews.mod" is a subset of "reviews", "trn.idx" does not contain the row
#   names, but the row index.
tst.idx = as.matrix(match(as.integer(rownames(reviews.mod))[-trn.idx], 
                          rownames(reviews.mod)))

# Create sub-sample index (for early modeling)
#   Note: This runs into similar issues as "tst.idx" above. The sub-sample must
#   not contain any of the rows in "tst.idx". The code below actually works as
#   it should, but to reference the subset of "reviews.mod", you must use
#   "reviews.mod[trn.idx, ][trn.idx.sub, ]" as "reviews.mod[trn.idx.sub, ]"
#   will produce an object which contains rows in "reviews.mod[tst.idx, ]".
set.seed(123)
trn.idx.sub = createDataPartition(reviews.mod$helpful.bins[trn.idx],
                                  p = 0.15,
                                  list = F)

#--------------------------------------
# Validate indexes
#--------------------------------------
# Train and test
temp = rbind(reviews.mod[trn.idx, ], 
             reviews.mod[tst.idx, ])
anyDuplicated(temp$reviewsPK); rm(temp)

# Train sub-set and test
temp = rbind(reviews.mod[trn.idx, ][trn.idx.sub, ],
             reviews.mod[tst.idx, ])
anyDuplicated(temp$reviewsPK); rm(temp)

#------------------------------------------------------------------------------
# Train-Test Datasets
#------------------------------------------------------------------------------
# Train set
reviews.mod.trn = reviews.mod[trn.idx, ]

# Train sub-set
reviews.mod.trn.sub = reviews.mod[trn.idx, ][trn.idx.sub, ]

# Test set
reviews.mod.tst = reviews.mod[tst.idx, ]

#==============================================================================
# S09 - Model Build (Classification)
#==============================================================================
# Note: various models built, organized by type, then model number(s)
# Note: models below currently use sub-sample to build model, but predict
#   on full test set

#------------------------------------------------------------------------------
# Random Forest (Parallel)
#------------------------------------------------------------------------------

#--------------------------------------
# Model 1 | train sub-sample | mtry
#--------------------------------------
# Note: model fits well in-sample (100% accuracy), but not out-of-sample
#   (61.89% accuracy, marginally above out-of-sample prevelance rate of 61.66%)
# Note: model run time ~30 seconds

# Use randomForest::tuneRF() for baseline model
#   mtry = 15
ptm = proc.time()
set.seed(55555)
rf.Tune = tuneRF(x = reviews.mod.trn.sub[, -c(1:14, 18)], 
                 y = reviews.mod.trn.sub[, 14],
                 ntreeTry = 150,
                 stepFactor = 1.5,
                 improve = 0.05,  
                 trace = TRUE,
                 plot = TRUE)
proc.time() - ptm; rm(ptm)

# Specify fit parameters
parRF.m1.fc = trainControl(method = "none")
parRF.m1.grid = data.frame(mtry = 15)

# Run model
registerDoParallel(2)
ptm = proc.time()
set.seed(55555)
parRF.m1 = train(x = reviews.mod.trn.sub[, -c(1:14, 18)], 
                 y = reviews.mod.trn.sub[, 14], 
                 method = "parRF", 
                 trControl = parRF.m1.fc, 
                 tuneGrid = parRF.m1.grid, 
                 preProcess = c("nzv", "center", "scale"), 
                 verbose = TRUE)
proc.time() - ptm; rm(ptm)
closeAllConnections()

# Summary information
parRF.m1
parRF.m1$finalModel
varImp(parRF.m1)
plot(varImp(parRF.m1))

# In-sample
parRF.m1.trn.pred = predict(parRF.m1,
                            newdata = reviews.mod.trn.sub)
parRF.m1.trn.cm = confusionMatrix(parRF.m1.trn.pred,
                                  reviews.mod.trn.sub$helpful.bins)

# Out-of-sample
parRF.m1.tst.pred = predict(parRF.m1,
                            newdata = reviews.mod.tst)
parRF.m1.tst.cm = confusionMatrix(parRF.m1.tst.pred,
                                  reviews.mod.tst$helpful.bins)

# Save model
save(parRF.m1, file = file.path(getwd(), "parRF.m1.RData"))

#--------------------------------------
# Model 2 | train sample | mtry
#--------------------------------------
# Note: model fits well in-sample (100% accuracy), but not out-of-sample
#   (63.73% accuracy, marginally above out-of-sample prevelance rate of 61.66%)
# Note: model run time ~3 mins

# Specify fit parameters
parRF.m2.fc = trainControl(method = "none")
parRF.m2.grid = data.frame(mtry = 15)

# Run model
registerDoParallel(2)
ptm = proc.time()
set.seed(55555)
parRF.m2 = train(x = reviews.mod.trn[, -c(1:14, 18)], 
                 y = reviews.mod.trn[, 14], 
                 method = "parRF", 
                 trControl = parRF.m2.fc, 
                 tuneGrid = parRF.m2.grid, 
                 preProcess = c("nzv", "center", "scale"), 
                 verbose = TRUE)
proc.time() - ptm; rm(ptm)
closeAllConnections()

# Summary information
parRF.m2
parRF.m2$finalModel
varImp(parRF.m2)
plot(varImp(parRF.m2))

# In-sample
parRF.m2.trn.pred = predict(parRF.m2,
                            newdata = reviews.mod.trn)
parRF.m2.trn.cm = confusionMatrix(parRF.m2.trn.pred,
                                  reviews.mod.trn$helpful.bins)

# Out-of-sample
parRF.m2.tst.pred = predict(parRF.m2,
                            newdata = reviews.mod.tst)
parRF.m2.tst.cm = confusionMatrix(parRF.m2.tst.pred,
                                  reviews.mod.tst$helpful.bins)

# Save model
save(parRF.m2, file = file.path(getwd(), "parRF.m2.RData"))

#--------------------------------------
# Model 3 | train sub-sample | cv
#--------------------------------------
# Note: model fits well in-sample (100% accuracy), but not out-of-sample
#   (61.97% accuracy, marginally above out-of-sample prevelance rate of 61.66%)
# Note: model run time ~17.5 mins

# Specify fit parameters
set.seed(55555)
parRF.m3.fc = trainControl(method = "cv",
                           returnResamp = "all",
                           verboseIter = TRUE)

# Run model
registerDoParallel(2)
ptm = proc.time()
set.seed(55555)
parRF.m3 = train(x = reviews.mod.trn.sub[, -c(1:14, 18)], 
                 y = reviews.mod.trn.sub[, 14],
                 method = "parRF",
                 trControl = parRF.m3.fc,
                 preProcess = c("nzv", "center", "scale"),
                 verbose = TRUE)
proc.time() - ptm; rm(ptm)
closeAllConnections()

# Summary information
parRF.m3
parRF.m3$finalModel
varImp(parRF.m3)
plot(varImp(parRF.m3))

# In-sample
parRF.m3.trn.pred = predict(parRF.m3,
                            newdata = reviews.mod.trn.sub)
parRF.m3.trn.cm = confusionMatrix(parRF.m3.trn.pred,
                                  reviews.mod.trn.sub$helpful.bins)

# Out-of-sample
parRF.m3.tst.pred = predict(parRF.m3,
                            newdata = reviews.mod.tst)
parRF.m3.tst.cm = confusionMatrix(parRF.m3.tst.pred,
                                  reviews.mod.tst$helpful.bins)

# Save model
save(parRF.m3, file = file.path(getwd(), "parRF.m3.RData"))

#--------------------------------------
# Model 4 | train sample | cv
#--------------------------------------
# Note: model fits well in-sample (100% accuracy), but not out-of-sample
#   (64.10% accuracy, marginally above out-of-sample prevelance rate of 61.66%)
# Note: model run time ~1.5 hours

# Specify fit parameters
set.seed(55555)
parRF.m4.fc = trainControl(method = "cv",
                           returnResamp = "all",
                           verboseIter = TRUE)

# Run model
registerDoParallel(2)
ptm = proc.time()
set.seed(55555)
parRF.m4 = train(x = reviews.mod.trn[, -c(1:14, 18)], 
                 y = reviews.mod.trn[, 14], 
                 method = "parRF", 
                 trControl = parRF.m4.fc, 
                 preProcess = c("nzv", "center", "scale"), 
                 verbose = TRUE)
proc.time() - ptm; rm(ptm)
closeAllConnections()

# Summary information
parRF.m4
parRF.m4$finalModel
varImp(parRF.m4)
plot(varImp(parRF.m4))

# In-sample
parRF.m4.trn.pred = predict(parRF.m4,
                            newdata = reviews.mod.trn)
parRF.m4.trn.cm = confusionMatrix(parRF.m4.trn.pred,
                                  reviews.mod.trn$helpful.bins)

# Out-of-sample
parRF.m4.tst.pred = predict(parRF.m4,
                            newdata = reviews.mod.tst)
parRF.m4.tst.cm = confusionMatrix(parRF.m4.tst.pred,
                                  reviews.mod.tst$helpful.bins)

# Save model
save(parRF.m4, file = file.path(getwd(), "parRF.m4.RData"))

#--------------------------------------
# Model 5 | train sub-sample | rcv
#--------------------------------------
# Note: model fits well in-sample (100% accuracy), but not out-of-sample
#   (61.68% accuracy, marginally above out-of-sample prevelance rate of 61.66%)
# Note: model run time ~47 mins

# Specify fit parameters
set.seed(55555)
parRF.m5.fc = trainControl(method = "repeatedcv",
                           repeats = 3,
                           returnResamp = "all",
                           verboseIter = TRUE)

# Run model
registerDoParallel(2)
ptm = proc.time()
set.seed(55555)
parRF.m5 = train(x = reviews.mod.trn.sub[, -c(1:14, 18)], 
                 y = reviews.mod.trn.sub[, 14],
                 method = "parRF",
                 trControl = parRF.m5.fc,
                 preProcess = c("nzv", "center", "scale"),
                 verbose = TRUE)
proc.time() - ptm; rm(ptm)
closeAllConnections()

# Summary information
parRF.m5
parRF.m5$finalModel
varImp(parRF.m5)
plot(varImp(parRF.m5))

# In-sample
parRF.m5.trn.pred = predict(parRF.m5,
                            newdata = reviews.mod.trn.sub)
parRF.m5.trn.cm = confusionMatrix(parRF.m5.trn.pred,
                                  reviews.mod.trn.sub$helpful.bins)

# Out-of-sample
parRF.m5.tst.pred = predict(parRF.m5,
                            newdata = reviews.mod.tst)
parRF.m5.tst.cm = confusionMatrix(parRF.m5.tst.pred,
                                  reviews.mod.tst$helpful.bins)

# Save model
save(parRF.m5, file = file.path(getwd(), "parRF.m5.RData"))

#--------------------------------------
# Model 6 | train sample | rcv
#--------------------------------------
# Note: model fits well in-sample (100% accuracy), but not out-of-sample
#   (64.16% accuracy, marginally above out-of-sample prevelance rate of 61.66%)
# Note: model run time ~4 hours, 45 mins

# Specify fit parameters
set.seed(55555)
parRF.m6.fc = trainControl(method = "repeatedcv",
                           repeats = 3,
                           returnResamp = "all",
                           verboseIter = TRUE)

# Run model
registerDoParallel(2)
ptm = proc.time()
set.seed(55555)
parRF.m6 = train(x = reviews.mod.trn[, -c(1:14, 18)], 
                 y = reviews.mod.trn[, 14], 
                 method = "parRF", 
                 trControl = parRF.m6.fc, 
                 preProcess = c("nzv", "center", "scale"), 
                 verbose = TRUE)
proc.time() - ptm; rm(ptm)
closeAllConnections()

# Summary information
parRF.m6
parRF.m6$finalModel
varImp(parRF.m6)
plot(varImp(parRF.m6))

# In-sample
parRF.m6.trn.pred = predict(parRF.m6,
                            newdata = reviews.mod.trn)
parRF.m6.trn.cm = confusionMatrix(parRF.m6.trn.pred,
                                  reviews.mod.trn$helpful.bins)

# Out-of-sample
parRF.m6.tst.pred = predict(parRF.m6,
                            newdata = reviews.mod.tst)
parRF.m6.tst.cm = confusionMatrix(parRF.m6.tst.pred,
                                  reviews.mod.tst$helpful.bins)

# Save model
save(parRF.m6, file = file.path(getwd(), "parRF.m6.RData"))

#--------------------------------------
# Model 7 | train sub-sample | oob
#--------------------------------------
# Note: model fits well in-sample (100% accuracy), but not out-of-sample
#   (61.73% accuracy, marginally above out-of-sample prevelance rate of 61.66%)
# Note: model run time ~3 mins

# Specify fit parameters
set.seed(55555)
parRF.m7.fc = trainControl(method = "oob",
                           returnResamp = "all",
                           verboseIter = TRUE)

# Run model
registerDoParallel(2)
ptm = proc.time()
set.seed(55555)
parRF.m7 = train(x = reviews.mod.trn.sub[, -c(1:14, 18)], 
                 y = reviews.mod.trn.sub[, 14],
                 method = "parRF",
                 trControl = parRF.m7.fc,
                 preProcess = c("nzv", "center", "scale"),
                 verbose = TRUE)
proc.time() - ptm; rm(ptm)
closeAllConnections()

# Summary information
parRF.m7
parRF.m7$finalModel
varImp(parRF.m7)
plot(varImp(parRF.m7))

# In-sample
parRF.m7.trn.pred = predict(parRF.m7,
                            newdata = reviews.mod.trn.sub)
parRF.m7.trn.cm = confusionMatrix(parRF.m7.trn.pred,
                                  reviews.mod.trn.sub$helpful.bins)

# Out-of-sample
parRF.m7.tst.pred = predict(parRF.m7,
                            newdata = reviews.mod.tst)
parRF.m7.tst.cm = confusionMatrix(parRF.m7.tst.pred,
                                  reviews.mod.tst$helpful.bins)

# Save model
save(parRF.m7, file = file.path(getwd(), "parRF.m7.RData"))

#--------------------------------------
# Model 8 | train sample | oob
#--------------------------------------
# Note: model fits well in-sample (100% accuracy), but not out-of-sample
#   (64.16% accuracy, marginally above out-of-sample prevelance rate of 61.66%)
# Note: model run time ~17 mins

# Specify fit parameters
set.seed(55555)
parRF.m8.fc = trainControl(method = "oob",
                           returnResamp = "all",
                           verboseIter = TRUE)

# Run model
registerDoParallel(2)
ptm = proc.time()
set.seed(55555)
parRF.m8 = train(x = reviews.mod.trn[, -c(1:14, 18)], 
                 y = reviews.mod.trn[, 14], 
                 method = "parRF", 
                 trControl = parRF.m8.fc, 
                 preProcess = c("nzv", "center", "scale"), 
                 verbose = TRUE)
proc.time() - ptm; rm(ptm)
closeAllConnections()

# Summary information
parRF.m8
parRF.m8$finalModel
varImp(parRF.m8)
plot(varImp(parRF.m8))

# In-sample
parRF.m8.trn.pred = predict(parRF.m8,
                            newdata = reviews.mod.trn)
parRF.m8.trn.cm = confusionMatrix(parRF.m8.trn.pred,
                                  reviews.mod.trn$helpful.bins)

# Out-of-sample
parRF.m8.tst.pred = predict(parRF.m8,
                            newdata = reviews.mod.tst)
parRF.m8.tst.cm = confusionMatrix(parRF.m8.tst.pred,
                                  reviews.mod.tst$helpful.bins)

# Save model
save(parRF.m8, file = file.path(getwd(), "parRF.m8.RData"))

#------------------------------------------------------------------------------
# Support Vector Machine
#------------------------------------------------------------------------------

# Lorem ipsum

#==============================================================================
# S10 - Model Build (Regression)
#==============================================================================
# Note: various models built, organized by type, then model number(s)
# Note: models below currently use sub-sample to build model, but predict
#   on full test set

#------------------------------------------------------------------------------
# Random Forest
#------------------------------------------------------------------------------

# Lorem ipsum

#------------------------------------------------------------------------------
# Support Vector Machine
#------------------------------------------------------------------------------

# Lorem ipsum

#==============================================================================
# FIN
#==============================================================================
sessionInfo()

