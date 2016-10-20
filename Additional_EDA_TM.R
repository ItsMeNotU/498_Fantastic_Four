#load data
con  <- "/Users/tobiamartens/Desktop/Fantastic Four - Capstone/Data/reviews_Grocery_and_Gourmet_Food_5.json"

reviews <- stream_in(file(con))

#count the number of words per review
numWords <- sapply(gregexpr("\\W+", reviews$reviewText), length)
numWords.5stars <- sapply(gregexpr("\\W+", reviews[reviews$overall==5,]$reviewText), length)
numWords.4stars <- sapply(gregexpr("\\W+", reviews[reviews$overall==4,]$reviewText), length)
numWords.3stars <- sapply(gregexpr("\\W+", reviews[reviews$overall==3,]$reviewText), length)
numWords.2stars <- sapply(gregexpr("\\W+", reviews[reviews$overall==2,]$reviewText), length)
numWords.1stars <- sapply(gregexpr("\\W+", reviews[reviews$overall==1,]$reviewText), length)

#average number of words per review: 97
overall.avg.numWords <- round(mean(numWords),0)
overall.avg.numWords

#average number of words for 5 star: 84
avg.numWords.5stars <- round(mean(numWords.5stars),0)
avg.numWords.5stars

#average number of words for 4 star: 113
avg.numWords.4stars <- round(mean(numWords.4stars),0 )
avg.numWords.4stars

#average number of words for 3 star: 119
avg.numWords.3stars <- round(mean(numWords.3stars), 0)
avg.numWords.3stars

#average number of words for 2 star: 114
avg.numWords.2stars <- round(mean(numWords.2stars), 0)
avg.numWords.2stars

#average number of words for 1 stars: 105
avg.numWords.1stars <- round(mean(numWords.1stars), 0)
avg.numWords.1stars

#plot the results
counts <- matrix(c(avg.numWords.1stars, avg.numWords.2stars, avg.numWords.3stars, 
                 avg.numWords.4stars, avg.numWords.5stars), ncol=1, nrow=5)
        
rownames(counts) <- c("OneStar", "TwoStar", "ThreeStar","FourStar", "FiveStar")
colnames(counts) <- c("Average Number of Words per Review")

barplot(counts[,1], main=colnames(counts), xlab= "Rating")
