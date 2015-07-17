rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    df <- read.csv("outcome-of-care-measures.csv", colClasses='character')
    ## Check that state and outcome are valid
    # check that the state exists
    if( !(state %in% df[, 7]) ) {
        stop('invalid state')
    }
    # check that the outcome column exists
    possible_outcomes <- c('heart attack', 'heart failure', 'pneumonia')
    if( ! outcome %in% possible_outcomes) {
        stop('invalid outcome')
    }
    # grab the index of outcome column
    idx <- NULL;
    if( outcome == 'heart attack') idx <- 11;
    if( outcome == 'heart failure') idx <- 17;
    if( outcome == 'pneumonia') idx <- 23;
    # coerce probability values into numerics and drop NaNs
    df[, idx] <- suppressWarnings(as.numeric(df[, idx]))
    df <- df[complete.cases(df[,idx]) & (df[, 7] == state), c(2, idx)]
    df <- df[ order(df[,2], df[,1]), 1]
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    if (num == 'best')
        res <- head(df, 1)
    else if (num == 'worst')
        res <- tail(df, 1)
    else {
        if (num > length(df)[1])
            res <- NA
        else
            res <- df[num]
        }
    res
}
