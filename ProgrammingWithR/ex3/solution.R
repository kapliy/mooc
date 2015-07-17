# solution for ex3
source("http://d396qusza40orc.cloudfront.net/rprog%2Fscripts%2Fsubmitscript3.R")
# rprog-030
# nDPACmxzAF

#outcome <- read.csv("outcome-of-care-measures.csv", colClasses='character')
#head(outcome)
#outcome[, 11] <- as.numeric(outcome[, 11])
#hist(outcome[, 11])
source('best.R')
stopifnot(best("TX", "heart attack") == "CYPRESS FAIRBANKS MEDICAL CENTER")
stopifnot(best("TX", "heart failure") == "FORT DUNCAN MEDICAL CENTER")
stopifnot(best("MD", "heart attack") == "JOHNS HOPKINS HOSPITAL, THE")
stopifnot(best("MD", "pneumonia") ==  "GREATER BALTIMORE MEDICAL CENTER")
best("BB", "heart attack")
best("NY", "hert attack")


source('rankhospital.R')
stopifnot(rankhospital("TX", "heart failure", 4) == "DETAR HOSPITAL NAVARRO")
stopifnot(rankhospital("MD", "heart attack", "worst") ==  "HARFORD MEMORIAL HOSPITAL")
stopifnot(is.na(rankhospital("MN", "heart attack", 5000)))

source('rankall.R')
head(rankall("heart attack", 20), 10)
tail(rankall("pneumonia", "worst"), 3)
tail(rankall("heart failure"), 10)