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

# Convert character features to numeric levels
recs$reviewerID = as.factor(recs$reviewerID)
recs$asin = as.factor(recs$asin)

# Convert factor levels to numeric values
levels(recs$reviewerID) = seq(from  = 1,
                              to = nlevels(recs$reviewerID),
                              by = 1)
levels(recs$asin) = seq(from = 1,
                        to = nlevels(recs$asin),
                        by = 1)

# Convert factor features to numeric
recs$reviewerID = as.numeric(as.character(recs$reviewerID))
recs$asin = as.numeric(as.character(recs$asin))

# Create flag for subset of recs
recs$helpful.nan = sapply(recs$helpful,
                          function(x) ifelse(x[2] == 0, 1, 
                                             ifelse(x[1]>x[2], 1, 0)))

#------------------------------------------------------------------------------
# Export
#------------------------------------------------------------------------------

# Subset recs for export
recs = recs[recs$helpful.nan == 0, ]

# Keep only three features
recs = subset(recs, select = c(reviewerID,
                               asin,
                               overall.num))

# Export for use in Mahout
write.table(x = recs,
            file = "recs.txt",
            quote = FALSE,
            sep = ",",
            col.names = FALSE,
            row.names = FALSE)

#==============================================================================
# FIN
#==============================================================================
sessionInfo()
