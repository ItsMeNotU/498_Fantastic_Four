# ==========================================================================
# Recommendation Engine - Version 1 - User-based Collaborative Filtering
# ==========================================================================
# Overview:
#               -Create User-Item Ranking Matrix
#               -Calculate similarity measure between users
#               -Recommend Products
# ==========================================================================
# 
#
# ==========================================================================
# Setup workspace, Load Packages, Data Import 
# ==========================================================================
# 
#
#Clear workspace
#
#
rm(list = ls())
#
#
# Load necessary packages
#
#
libs = c("jsonlite", "stats","recommenderlab","reshape2")
lapply(libs, require, character.only = TRUE)
#
#
# Set Working Directory & Import Data
#
#
setwd("/Users/tobiamartens/Desktop/Fantastic Four - Capstone/Data")
#
#
reviews = stream_in(file("reviews_Grocery_and_Gourmet_Food_5.json"))
#
#
# Subset the data for faster processing in initial run
#
#
dim(reviews)
#
#
# ==========================================================================
# Set up User-Item Ranking Matrix; every row is a user;
# every column is an item; observations are the ranking
# ==========================================================================
#
#
# Getting the dimensions for the matrix (8713 x 14681), setting 0 to NA
#
#
UserItem.Matrix = dcast(reviews, reviews$reviewerID~reviews$asin, value.var="overall")
#
#
UserItem.Matrix$`reviews$reviewerID` = NULL
#
#
UserItem.Matrix = as.matrix(UserItem.Matrix)
#
#
UserItem.Matrix[1:10,1:10]
#
#
# ==========================================================================
# Create a user based similarity model; calculates the cosine similarity 
# between all users represented as vectors
# Package: recommenderlabs
# Method: UBCF
# Similarity Calculation Method: Cosine Similarity
# Nearest Neighbors: 30
# ==========================================================================
#
# 
affinity.matrix = as(UserItem.Matrix,"realRatingMatrix")
#
#
affinity.matrix = normalize(affinity.matrix)
#
#
recommender_model = Recommender(affinity.matrix, method = "UBCF", 
                                 param=list(method="Cosine",
                                            nn=30))
#
# produces a topNlist
recom = predict(recommender_model, affinity.matrix[1], n=10) 
#
# force it as a list
recom_list = as(recom, "list") 
#
#
recom_list
#
#
# ==========================================================================
# Evaluating the Recommender System using a given 1 protocol:
# For each user the ratings for x randomly chosen item are given
# to the recommender algorithm to learn the model while remaining items
# are withheld for evaluation. For all but x, the algorithm is trained with 
# all but x withheld ratings.
# ==========================================================================
#
#
evaluation_scheme <- evaluationScheme(affinity.matrix, method="cross-validation", 
                                      k=5, 
                                      given=2, 
                                      goodRating=5)
#
#
evaluation_results <- evaluate(evaluation_scheme, method="UBCF", n=c(1,3,5,10,15,20))
#
#
eval_results <- getConfusionMatrix(evaluation_results)[[1]]
#
#
eval_results

