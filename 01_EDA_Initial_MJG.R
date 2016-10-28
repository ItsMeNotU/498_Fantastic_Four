###############################################################################
# initial_EDA_reviews_MJG
# Last Updated: 2016-10-16 by MJG
###############################################################################

library(dplyr)
library(jsonlite)
library(Rcampdf)
library(tm)

#==============================================================================
# Functions
#==============================================================================
# Source functions from GitHub
source.GitHub = function(url){
    require(RCurl)
    sapply(url, function(x){
        eval(parse(text = getURL(x, followlocation = T,
                   cainfo = system.file("CurlSSL", "cacert.pem",
                   package = "RCurl"))),
             envir = .GlobalEnv)
    })
}

# Assign URL and source functions
url = "http://bit.ly/1T6LhBJ"
source.GitHub(url); rm(url)

#==============================================================================
# Data Import, Prep, and Staging
#==============================================================================

# Load data
reviews = stream_in(file("reviews_Grocery_and_Gourmet_Food_5.json.gz"))

# View structure
str(reviews)

# Convert existing variables
reviews$reviewerID = as.factor(reviews$reviewerID)
reviews$asin = as.factor(reviews$asin)
reviews$overall = as.factor(reviews$overall)

# Create new variables
reviews$time.stamp = as.Date(as.POSIXct(reviews$unixReviewTime,
                                        origin="1970-01-01"))
reviews$weekday = as.factor(weekdays(reviews$time.stamp))
reviews$months = as.factor(months(reviews$time.stamp))
reviews$year = as.factor(substr(reviews$time.stamp, 1, 4))

#==============================================================================
# EDA
#==============================================================================

#------------------------------------------------------------------------------
# Quantitative
#------------------------------------------------------------------------------

# Frequency of overall ratings
fac.freq(reviews$overall, cat = F)

# Frequency of reviewerID
t(fac.freq(reviews$reviewerID, cat = F))

# Frequency of asin
t(fac.freq(reviews$asin, cat = F))

# Frequency of ratings by reviewerID
freq.reviewerID = fac.freq(reviews$reviewerID, reviews$overall)
View(freq.reviewerID)

# Frequency of ratings by asin
freq.asin = fac.freq(reviews$asin, reviews$overall)
View(freq.asin)

# Frequency of ratings by year
freq.year = fac.freq(reviews$year, reviews$overall)
freq.year

#------------------------------------------------------------------------------
# Qualitative
#------------------------------------------------------------------------------

# Frequency of overall ratings
fac.barplot(reviews$overall)

# Histogram of overall ratings
ggplot(data = reviews) + 
    geom_histogram(aes(x = as.numeric(overall)), fill = "grey50") + 
    ggtitle("Histogram of Amazon Product Ratings")

# Reviews over time
reviews_by_day = as.data.frame(reviews %>% count(time.stamp, sort=TRUE))
reviews_by_day = reviews_by_day[order(reviews_by_day$time.stamp),]

g = ggplot(reviews_by_day, aes(x = time.stamp, y = n)) + geom_line()
g = g + labs(title = "Number of Reviews over time",
              y = "Number \n of Reviews", x = "Time")
g

#==============================================================================
# FIN
#==============================================================================
