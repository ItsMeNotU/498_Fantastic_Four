
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
# Import
#------------------------------------------------------------------------------
# Load data

#platform
# TRUE  = "win"
# FALSE = "mac"
platform = FALSE
fName5CReviews ="reviews_Grocery_and_Gourmet_Food_5.json"
fNameRatings="ratings_Grocery_and_Gourmet_Food.csv"
fName.metadata ="metadata.json"
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
#con  <- paste0(path,fName.metadata)
#metadata <- stream_in(file(con))
# Add a primary key to use for convenience
reviews$reviewsPK = seq.int(nrow(reviews))

# Reorder to put reviewsPK first
reviews = reviews[c(10, 1:9)]
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
#Prep for charts
products.with.helpful.reviews = reviews %>%
  filter(helpful.bins=="Upper") %>%
  #group_by (asin) %>%
  distinct(asin, 
           .keep_all = TRUE) %>%
  mutate (helpful.ratio=helpful.up/helpful.total) %>%
  arrange(desc(helpful.ratio),desc(helpful.up)) %>%
  select (asin,helpful.up,helpful.total)

#==============================================================================
# S04 - EDA
#==============================================================================

# Subset reviews for EDA
reviews.eda = reviews[reviews$helpful.nan == 0, ]
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








