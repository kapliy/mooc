rankall <- function(outcome, num = "best") {
    ## Read outcome data
    df <- read.csv("outcome-of-care-measures.csv", colClasses='character')
    ## Check that outcome is valid
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
    
    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    
    do_state <- function(state, idx, df) {
        ddf <- df[complete.cases(df[,idx]) & (df[, 7] == state), c(2, idx)]
        ddf <- ddf[ order(ddf[,2], ddf[,1]), 1]
        ## For each state, find the hospital of the given rank
        ## Return a data frame with the hospital names and the
        # TODO
        ## (abbreviated) state name
        # TODO
        if (num == 'best')
            res <- head(ddf, 1)
        else if (num == 'worst')
            res <- tail(ddf, 1)
        else {
            if (num > length(ddf)[1])
                res <- NA
            else
                res <- ddf[num]
        }
        res
    }
    
    nn <- length(unique(df[, 7]))
    chospital <- vector('character', nn)
    cstate <- vector('character', nn)
    ii <- 0
    for (state in unique(df[, 7])) {
        chospital[ii] <- do_state(state, idx, df)
        cstate[ii] <- state
        ii <- ii + 1
    }
    state <- cstate
    hospital <- chospital
    out <- data.frame(state, hospital)
    out[ order(out[,1]), ]
}

