#==============================================================================
#==============================================================================
# 00_recs_Master
# Last Updated: 2016-11-17 by MJG
#==============================================================================
#==============================================================================

# Clear workspace
rm(list=ls())

# Load libraries
library(jsonlite)

#==============================================================================
# Data Import, Quality Check, Prep, and Export
#==============================================================================

#------------------------------------------------------------------------------
# Import
#------------------------------------------------------------------------------
# Load data
recs = stream_in(file("reviews_Grocery_and_Gourmet_Food_5.json.gz"))

#------------------------------------------------------------------------------
# Quality Check
#------------------------------------------------------------------------------
# Check for duplicated rows
anyDuplicated(recs,
              fromLast = TRUE)
anyDuplicated(recs[c("reviewerID", "asin")],
              fromLast = TRUE)

# Add a primary key to use for convenience
recs$recsPK = seq.int(nrow(recs))

# Reorder to put recsPK first
recs = recs[c(10, 1:9)]

#------------------------------------------------------------------------------
# Prep
#------------------------------------------------------------------------------
# Rename existing variables
names(recs)[names(recs)=="overall"] = "overall.num"

# Convert character features to factors
recs$reviewerID = as.factor(recs$reviewerID)
recs$asin = as.factor(recs$asin)

# Create numeric versions of factors
recs$reviewerID.num = recs$reviewerID
recs$asin.num = recs$asin

# Convert factor levels to numeric values
levels(recs$reviewerID.num) = seq(from  = 1,
                                  to = nlevels(recs$reviewerID.num),
                                  by = 1)
levels(recs$asin.num) = seq(from = 1,
                            to = nlevels(recs$asin.num),
                            by = 1)

# Convert factor features to numeric
recs$reviewerID.num = as.numeric(as.character(recs$reviewerID.num))
recs$asin.num = as.numeric(as.character(recs$asin.num))

# Create flag for subset of recs
recs$helpful.nan = sapply(recs$helpful,
                          function(x) ifelse(x[2] == 0, 1, 
                                             ifelse(x[1]>x[2], 1, 0)))

#------------------------------------------------------------------------------
# Export
#------------------------------------------------------------------------------
# Subset recs for export
recs = recs[recs$helpful.nan == 0, ]

# Export for use in Mahout
write.table(x = recs[, c(11, 12, 7)],
            file = "recs.txt",
            quote = FALSE,
            sep = ",",
            col.names = FALSE,
            row.names = FALSE)

#==============================================================================
# Mahout Results
#==============================================================================

# Load recommendations
temp = read.csv(file = "part-r-00000",
                header = FALSE,
                sep = "\t",
                stringsAsFactors = FALSE)

# Remove brackets
temp$V2 = gsub(pattern = "\\[|\\]",
               replacement = "",
               x = temp$V2)

# Reshape from semi-wide to long
recs.out = data.frame()
for (row in 1:nrow(temp)){
    V1 = temp$V1[row]
    V2 = unlist(strsplit(x = temp$V2[row],
                         split = ",",
                         fixed = TRUE))
    recs.out = rbind(recs.out,
                     cbind(rep(x = V1,
                               times = length(V2)),
                           V2))
}

# Only keep asin value (not score)
recs.out$V2 = gsub(pattern = "^(.*?):.*",
                   replacement = "\\1",
                   x = recs.out$V2)

# Change columns back to numeric for join
recs.out$V1 = as.numeric(as.character(recs.out$V1))
recs.out$V2 = as.numeric(as.character(recs.out$V2))

# Rename columns
colnames(recs.out) = c("reviewerID.num",
                       "asin.num")

# Clean up
rm(temp); rm(row); rm(V1); rm(V2)

#==============================================================================
# Recommendations Dataset
#==============================================================================

# Replace numeric values with original values
recs.out$reviewerID = recs$reviewerID[match(recs.out$reviewerID.num,
                                            recs$reviewerID.num)]
recs.out$asin = recs$asin[match(recs.out$asin.num,
                                recs$asin.num)]

# Drop numeric versions
recs.out = recs.out[, 3:4]

# Export
write.table(x = recs.out,
            file = "recs_out.csv",
            quote = FALSE,
            sep = ",",
            row.names = FALSE)

# Clean up
rm(list = ls(pattern = "recs"))

#==============================================================================
# FIN
#==============================================================================
sessionInfo()
