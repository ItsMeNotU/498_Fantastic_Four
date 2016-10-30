##Ref: https://www.r-bloggers.com/accessing-apis-from-r-and-a-little-r-programming/
library(httr)
library(jsonlite)

options(stringsAsFactors = FALSE)

url  <- "http://search-predict-498-etrnmkcw2ss5k2tne664bemmjq.us-west-2.es.amazonaws.com/"
path <- "reviews/_search?q=coffee"

raw.result <- GET(url = url, path = path)
this.raw.content <- rawToChar(raw.result$content)

this.content <- fromJSON(this.raw.content)



