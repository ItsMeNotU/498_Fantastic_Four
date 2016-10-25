#load data
con  <- "/Users/tobiamartens/Desktop/Fantastic Four - Capstone/Data/reviews_Grocery_and_Gourmet_Food_5.json"

reviews <- stream_in(file(con))

#count the number of words per review, length returns 1 if there is a match
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

#sample a 4 & 5 star sample and a 3 and below star sample
four.five <- reviews[reviews$overall %in% c(4,5), ]
rows <- sample(nrow(four.five), 5000)
four.five <- four.five[rows, ]

three.below <- reviews[reviews$overall %in% c(1,2,3), ]
rows <- sample(nrow(three.below), 5000)
three.below <- three.below[rows, ]


#create a corpus from the 5000 sampled reviews
library(tm)

corpus.1 <- Corpus(VectorSource(four.five$reviewText)) 
corpus.2 <- Corpus(VectorSource(three.below$reviewText))


#remove whitespace, convert to lower case, remove stop words (english & custom), and stem
clean.text <- function(corpus){
        corpus <- tm_map(corpus, removePunctuation)
        corpus <- tm_map(corpus, content_transformer(tolower))
        corpus <- tm_map(corpus, removeWords, c(stopwords("english"),"taste",
                                                "Taste","like","flavor",
                                                "just","one","good",
                                                "tea","product","the","coffee",
                                                "can","The"))
        corpus <- tm_map(corpus, stripWhitespace)
        corpus <- tm_map(corpus, stemDocument)
        corpus
}

corpus.1 <- clean.text(corpus.1)
corpus.2 <- clean.text(corpus.2)

#after documents are cleaned, we load them into a term document matrix. The matrix
#presents the number of times unique term features in each distinct document
dtm1 <- DocumentTermMatrix(corpus.1)
dtm2 <- DocumentTermMatrix(corpus.2)


#most frequent terms
dtm.1 <- data.frame(as.matrix(dtm1))
dtm.2 <- data.frame(as.matrix(dtm2))

v1 <- as.matrix(sort(sapply(dtm.1, "sum"), decreasing = TRUE)[1:length(dtm.1)], 
                colnames = count)
v2 <- as.matrix(sort(sapply(dtm.2, "sum"), decreasing = TRUE)[1:length(dtm.2)], 
                colnames = count)

five.four <- data.frame(v1)
three.two.one <- data.frame(v2)

#merge the two together and set NAs to 0
wordsCompare <- merge(five.four, three.two.one, by = "row.names", all = TRUE)
wordsCompare[is.na(wordsCompare)] <- 0

#zscore
wordsCompare$prop <- wordsCompare$v1/sum(wordsCompare$v1)
wordsCompare$prop2 <- wordsCompare$v2/sum(wordsCompare$v2)
wordsCompare$z <- (wordsCompare$prop - wordsCompare$prop2)/((sqrt(((sum(wordsCompare$v1) * 
                                                                            wordsCompare$prop) + (sum(wordsCompare$v2) * wordsCompare$prop2))/(sum(wordsCompare$v1) + 
                                                                                                                                                       sum(wordsCompare$v2)) * (1 - ((sum(wordsCompare$v1) * wordsCompare$prop) + 
                                                                                                                                                                                             (sum(wordsCompare$v2) * wordsCompare$prop2))/(sum(wordsCompare$v1) + sum(wordsCompare$v2))))) * 
                                                                    (sqrt((sum(wordsCompare$v1) + sum(wordsCompare$v2))/(sum(wordsCompare$v1) * 
                                                                                                                                 sum(wordsCompare$v2)))))




wordsCompare<-subset(wordsCompare,abs(z)>1.96)

wordsCompare <- wordsCompare[order(abs(wordsCompare$z), decreasing = T), ]  #order according to significance

wordsCompare$dif1 <- -100 * (1 - wordsCompare$prop/wordsCompare$prop2)
# calculate percentage increase
wordsCompare$dif2 <- 100 * (1 - wordsCompare$prop2/wordsCompare$prop)

# merge result 
wordsCompare$dif <- 0
wordsCompare$dif[wordsCompare$dif1 < 0] <- wordsCompare$dif1[wordsCompare$dif1 < 
                                                                     0]
wordsCompare$dif[wordsCompare$dif2 > 0] <- wordsCompare$dif2[wordsCompare$dif2 > 
                                                                     0]
wordsCompare$dif1 <- NULL
wordsCompare$dif2 <- NULL
require(ggplot2)
ggplot(wordsCompare, aes(z, dif)) + geom_point()


require(ggplot2)
wordsCompare$z2 <- 1
wordsCompare$z2[abs(wordsCompare$z)>=1.96] <- 0
wordsCompare <- wordsCompare[wordsCompare$z>-99&wordsCompare$z<99,]

wordsCompare <- wordsCompare[order(abs(wordsCompare$prop2+wordsCompare$prop),decreasing=T),] 
ggplot(head(wordsCompare,50), #select number of points in plot
       aes(dif,log(abs(v1+v2)),label=Row.names,size=(v1+v2),colour=z2))+ #plot z value against difference
        geom_text(fontface=2,alpha=.8)+
        scale_size(range = c(3, 12))+#specify scale
        ylab("Log number of mentions")+
        xlab("Percentage difference betwen samples\n <---------More in Reviews with Lower Rating ------|-----More in Reviews with Higher Rating-------->")+
        geom_vline(xintercept = 0,colour="red",linetype=2)+theme_bw()+theme(legend.position="none")+
        ggtitle("Terms Featured")


