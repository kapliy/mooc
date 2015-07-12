# cr <- corr("specdata", 150)

corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    ## NOTE: Do not round the result!
    out <- numeric(0)
    fnames <- list.files('specdata', pattern='*.csv')
    for (i in seq_along(fnames)) {
        fname <- paste('specdata/', fnames[i], sep='')
        df <- na.omit(read.csv(fname, header=T))
        # print(fname)
        rr <- nrow(df)
        if (rr >= threshold) {
            colA <- names(df)[2]
            colB <- names(df)[3]
            cc <- cor(df[, colA], df[, colB])
            out <- c(out, cc)
        }
    }
    na.omit(out)
}
