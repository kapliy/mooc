# pollutantmean('specdata', 'sulfate', 1:2)
# pollutantmean("specdata", "nitrate", 70:72)

pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    ## NOTE: Do not round the result!
    tot <- numeric(0)
    for (i in seq_along(id)) {
        fname <- paste('specdata/', sprintf("%03d", id[i]), '.csv', sep='')
        df <- read.csv(fname, header=T)
        #print(fname)
        #print(colnames(df))
        #print(pollutant)
        tot <- c(tot, df[!is.na(df[[pollutant]]), pollutant] )
    }
    mean(tot, na.rm=T)
}

