#==============================================================================
#==============================================================================
# reviews_Master
# Last Updated: 2016-10-27 by MJG
#==============================================================================
#==============================================================================

# Clear workspace
rm(list=ls())

# Load libraries
library(caret)
library(dplyr)
library(ggplot2)
library(jsonlite)
library(tm)
library(wordcloud)

#==============================================================================
# Functions
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
# Text Cleaning & Frequent Terms
#------------------------------------------------------------------------------
text.clean = function(df, freq = FALSE, stop.words){
    temp = Corpus(VectorSource(paste(df, collapse = " ")))
    temp = tm_map(temp, content_transformer(tolower))
    temp = tm_map(temp, removePunctuation)
    temp = tm_map(temp, stripWhitespace)
    if (!missing(stop.words)){
        temp = tm_map(temp, removeWords, c(stopwords("english"),
                                           stop.words))
    } else {
        temp = tm_map(temp, removeWords, c(stopwords("english")))
    }
    temp = tm_map(temp, stemDocument)
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

#==============================================================================
# Data Import, Prep, and Staging
#==============================================================================

#------------------------------------------------------------------------------
# Import
#------------------------------------------------------------------------------
# Load data
reviews = stream_in(file("reviews_Grocery_and_Gourmet_Food_5.json.gz"))

# View structure
str(reviews)

#------------------------------------------------------------------------------
# Prep
#------------------------------------------------------------------------------
# Rename existing variables
names(reviews)[names(reviews)=="overall"] = "overall.num"

# Create flags, percentage score, and bins for reviews$helpful
reviews$helpful.nan = sapply(reviews$helpful,
                             function(x) ifelse(x[2]==0, 1, 
                                             ifelse(x[1]>x[2], 1, 0)))
reviews$helpful.perc = sapply(reviews$helpful,
                              function(x) ifelse(x[2]==0, NA, 
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
reviews$reviewText.count = sapply(gregexpr("\\W+",
                                           reviews$reviewText),
                                  length)
reviews$summary.count = sapply(gregexpr("\\W+",
                                        reviews$summary),
                               length)
reviews$time.stamp = as.Date(as.POSIXct(reviews$unixReviewTime,
                                        origin="1970-01-01"))
reviews$time.weekday = as.factor(weekdays(reviews$time.stamp))
reviews$time.months = as.factor(months(reviews$time.stamp))
reviews$time.year = as.factor(substr(reviews$time.stamp, 1, 4))

#------------------------------------------------------------------------------
# Staging
#------------------------------------------------------------------------------
# Subset reviews for EDA, text mining, and modeling
reviews.mod = reviews[reviews$helpful.nan == 0, ]

#==============================================================================
# EDA
#==============================================================================
# NOTE: this only examines reviews which received a vote for helpfulness

#------------------------------------------------------------------------------
# Traditional - Quantitative
#------------------------------------------------------------------------------
# Frequency of reviewerID
freq.reviewerID = t(fac.freq(reviews.mod$reviewerID,
                             cat = F))

# Frequency of asin
freq.asin = t(fac.freq(reviews.mod$asin,
                       cat = F))

#--------------------------------------
# helpful.bins
#--------------------------------------
# Frequency of helpful bins
freq.helpful = fac.freq(reviews.mod$helpful.bins,
                        cat = F)

# Frequency of helpful bins by reviewerID
freq.helpful.reviewerID = fac.freq(reviews.mod$reviewerID, 
                                   reviews.mod$helpful.bins)

# Frequency of helpful bins by asin
freq.helpful.asin = fac.freq(reviews.mod$asin,
                             reviews.mod$helpful.bins)

# Frequency of helpful bins by year
freq.year = fac.freq(reviews.mod$time.year,
                     reviews.mod$helpful.bins)

# Count of upper helpful bin by asin
count.helpful.up = reviews.mod %>%
                       group_by(asin) %>%
                       filter(helpful.bins == "Upper") %>%
                       count(helpful.bins, 
                             sort = TRUE)

#--------------------------------------
# overall
#--------------------------------------
# Frequency of overall
freq.overall = fac.freq(reviews.mod$overall.fac,
                        cat = F)

# Frequency of overall by reviewerID
freq.overall.reviewerID = fac.freq(reviews.mod$reviewerID,
                                   reviews.mod$overall.fac)

# Frequency of overall by asin
freq.overall.asin = fac.freq(reviews.mod$asin,
                             reviews.mod$overall.fac)

# Frequency of overall by year
freq.overall.year = fac.freq(reviews.mod$time.year,
                             reviews.mod$overall.fac)

# Count of overall by asin for 5-star reviews
count.overall.fs = reviews.mod %>%
                       group_by(asin) %>%
                       filter(overall.fac == "5") %>%
                       count(overall.fac, 
                             sort = TRUE)

#------------------------------------------------------------------------------
# Traditional - Qualitative
#------------------------------------------------------------------------------
# Histogram of overall ratings
ggplot(data = reviews.mod,
       aes(x = overall.fac)) +
    geom_bar(fill = "grey50") +
    labs(title = "Histogram of Amazon Ratings by Overall",
         y = "Count",
         x = "Overall Rating")

# Histogram of overall ratings by year
ggplot(data = reviews.mod,
       aes(x = overall.fac)) +
    geom_bar(fill = "grey50") +
    facet_wrap(~time.year) +
    labs(title = "Histogram of Amazon Ratings by Overall",
         y = "Count",
         x = "Overall Rating")

# Histogram of helpful bins
ggplot(data = reviews.mod,
       aes(x = helpful.bins)) +
    geom_bar(fill = "grey50") +
    labs(title = "Histogram of Amazon Ratings by Helpfulness Bin",
         y = "Count",
         x = "Helpful Bins")

# Histogram of helpful bins by year
ggplot(data = reviews.mod,
       aes(x = helpful.bins)) +
    geom_bar(fill = "grey50") +
    facet_wrap(~time.year) +
    labs(title = "Histogram of Amazon Ratings by Helpfulness Bin",
         y = "Count",
         x = "Helpful Bins")

# Count of reviews over time
ggplot(data = reviews.mod %>%
                  count(time.stamp,
                        sort = FALSE),
       aes(x = time.stamp,
           y = n)) +
    geom_line() +
    labs(title = "Number of Reviews Over Time",
         y = "Number of Reviews",
         x = "Time")

#------------------------------------------------------------------------------
# Text Analysis
#------------------------------------------------------------------------------

#--------------------------------------
# Word Counts (Mean)
#--------------------------------------

#------------------
# helpful.bins
#------------------
# Mean word count for review text by helpful bins
mean.reviewText.lo = round(mean(reviews.mod$reviewText.count
                                [reviews.mod$helpful.bins == "Lower"]),
                           0)
mean.reviewText.md = round(mean(reviews.mod$reviewText.count
                                [reviews.mod$helpful.bins == "Middle"]),
                           0)
mean.reviewText.up = round(mean(reviews.mod$reviewText.count
                                [reviews.mod$helpful.bins == "Upper"]),
                           0)
count.reviewText = data.frame(Levels = levels(reviews.mod$helpful.bins),
                              Count = c(mean.reviewText.lo,
                                        mean.reviewText.md,
                                        mean.reviewText.up))

#------------------
# summary
#------------------
# Mean word count for summary by helpful bins
mean.summary.lo = round(mean(reviews.mod$summary.count
                             [reviews.mod$helpful.bins == "Lower"]),
                        0)
mean.summary.md = round(mean(reviews.mod$summary.count
                             [reviews.mod$helpful.bins == "Middle"]),
                        0)
mean.summary.up = round(mean(reviews.mod$summary.count
                             [reviews.mod$helpful.bins == "Upper"]),
                        0)
count.summary = data.frame(Levels = levels(reviews.mod$helpful.bins),
                           Count = c(mean.summary.lo,
                                     mean.summary.md,
                                     mean.summary.up))

#--------------------------------------
# Top 100 Frequent Terms
#--------------------------------------
# Overall
mft = text.clean(reviews.mod$reviewText,
                 freq = TRUE)

# Lower helpful bin
mft.lo = text.clean(reviews.mod$reviewText
                    [reviews.mod$helpful.bins == "Lower"],
                    freq = TRUE)

# Middle helpful bin
mft.md = text.clean(reviews.mod$reviewText
                    [reviews.mod$helpful.bins == "Middle"],
                    freq = TRUE)

# Upper helpful bin
mft.up = text.clean(reviews.mod$reviewText
                    [reviews.mod$helpful.bins == "Upper"],
                    freq = TRUE)

#--------------------------------------
# Word Clouds
#--------------------------------------
# Overall
wordcloud(names(mft), mft)

# Lower helpful bin
wordcloud(names(mft.lo), mft.lo)

# Middle helpful bin
wordcloud(names(mft.md), mft.md)

# Upper helpful bin
wordcloud(names(mft.up), mft.up)

#--------------------------------------
# Text Cleaning
#--------------------------------------
# Create stop list based on frequencies and word clouds
stop.list = c("tast",
              "like",
              "flavor",
              "just",
              "one",
              "good",
              "tea",
              "product",
              "the",
              "coffe",
              "can")

# Clean text using stop words
reviews.mod.corpus = text.clean(reviews.mod$reviewText,
                                stop.words = stop.list)

# Create Term Document Matrix
reviews.mod.tdm = TermDocumentMatrix(reviews.mod.corpus)

# Remove sparse terms
reviews.mod.tdm = removeSparseTerms(reviews.mod.tdm, 0.99)

#------------------------------------------------------------------------------
# Model-based
#------------------------------------------------------------------------------

# Split data
set.seed(123)
trn.idx.eda = createDataPartition(reviews.mod$helpful.bins,
                                  p = 0.25,
                                  list = F)

# Validate split; target = 0.25
length(trn.idx.eda) / nrow(reviews.mod)

#==============================================================================
# Model Build
#==============================================================================

# Split data
set.seed(123)
trn.idx = createDataPartition(reviews.mod$helpful.bins,
                              p = 0.70,
                              list = F)

# Validate split; target = 0.70
length(trn.idx) / nrow(reviews.mod)

#------------------------------------------------------------------------------
# Model_Type_01
#------------------------------------------------------------------------------

# Lorem ipsum

#------------------------------------------------------------------------------
# Model_Type_02
#------------------------------------------------------------------------------

# Lorem ipsum

#==============================================================================
# FIN
#==============================================================================
sessionInfo()
