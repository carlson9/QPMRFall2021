## Load libraries and set working directory
library(devtools)
library(roxygen2)
library(jsonlite)
library(httr)
library(plyr)
library(testthat)


setwd("~/SentimentIt")
# This will need to be changed to match your directory of where the package is.
## This is run once when the package strcuture is first created


## At this point put the *.R files into the correct directories and edit the DESCRIPTION file
current.code <- as.package("sentimentIt")
load_all(current.code)
document(current.code)
check(current.code)
build(current.code)


