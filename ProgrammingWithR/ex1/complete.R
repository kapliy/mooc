# complete("specdata", c(2, 4, 8, 10, 12))

complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    ids <- integer(length(id))
    obs <- integer(length(id))
    for (i in seq_along(id)) {
        fname <- paste('specdata/', sprintf("%03d", id[i]), '.csv', sep='')
        df <- read.csv(fname, header=T)
        #print(fname)
        #print(colnames(df))
        #print(pollutant)
        ids[i] <- id[i]
        obs[i] <- nrow(na.omit(df))
    }
    id <- ids;
    nobs <- obs;
    data.frame(id, nobs)
}
