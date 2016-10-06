###Intial EDA for Customer X Reviews###

#install.packages("jsonlite")
#install.packages("dplyr")
#install.packages("ggplot2")

library(jsonlite)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(RCurl)

#change to your directory where the data is located
con  <- "/Users/tobiamartens/Desktop/Fantastic Four - Capstone/Data/reviews_Grocery_and_Gourmet_Food_5.json"

reviews <- stream_in(file(con))

str(reviews)

mean(reviews$overall)

#how many unique products are there
length(unique(reviews$asin))

#The average number of reviews per product is 17.359, the max is 742, the min is 5
asin_freq <- reviews %>% count(asin, sort = TRUE)
mean(asin_freq$n)
max(asin_freq$n)
min(asin_freq$n)

#create factor ratings variable
reviews$overall.factor <- as.factor(reviews$overall)

#frequency table of ratings
table(reviews$overall.factor)

#plot a histogram of reviews

ggplot(data = reviews) + geom_histogram(aes(x = overall), fill = "grey50") + 
        ggtitle("Histogram of Amazon Product Ratings")

#Which product has the most positive reviews
totals <- as.data.frame(reviews %>% group_by(asin) %>% summarize(total = sum(overall)))
sorted.totals <- totals[order(-totals[,2]),]

#most upvoted products
head(sorted.totals,20)

#most downvoted products
tail(sorted.totals,20)

#which products has the most 5-star ratings
five_stars <- subset(reviews, overall.factor == 5, select = c(reviewerID, asin))

freq_five_stars <- five_stars %>% count(asin, sort=TRUE)

#amazon product IDs with the most number of 5-star ratings
head(freq_five_stars,20)

#is there a relationship between time, reviews, & rating?
head(reviews$unixReviewTime)
head(reviews$reviewTime)

reviews$time.stamp <- as.Date(as.POSIXct(reviews$unixReviewTime, 
                                 origin="1970-01-02"))

reviews_by_day <- as.data.frame(reviews %>% count(time.stamp, sort=TRUE))
reviews_by_day <- reviews_by_day[order(reviews_by_day$time.stamp),]

g <- ggplot(reviews_by_day, aes(x = time.stamp, y = n)) + geom_line()
g <- g + labs(title = "Number of Reviews over time",
              y = "Number \n of Reviews", x = "Time")
g

#average ratings by day, by month - no significant differences
reviews$weekday <- weekdays(reviews$time.stamp)
reviews$months <- months(reviews$time.stamp)
reviews$year <- substr(reviews$time.stamp,1,4)

by_weekday <- as.data.frame(reviews %>% group_by(weekday) %>% summarize(mean(overall)))
by_weekday

by_month <- as.data.frame(reviews %>% group_by(months) %>% summarize(mean(overall)))
by_month

ggplot(data = reviews, aes(x = overall)) + geom_histogram() +
        facet_wrap(~year)

t <- table(reviews$year, as.factor(reviews$overall))
t <- t[-c(1,2,3,4),]

row_probs <- matrix(nrow = dim(t)[1], ncol = dim(t)[2])

for (i in 1:nrow(t)){
        for (r in 1:ncol(t)){
                row_probs[i,r] <- t[i,r] / sum(t[i,])
        }
}

rownames(row_probs) <- rownames(t)
colnames(row_probs) <- colnames(t)

row_probs
