pollutantmean <- function(directory, pollutant, id=1:332)
{
    pollut_sum <- 0
    pollut_num <- 0
    
    for (i in id)
    {
        # read data from .csv
        data<-read.csv(paste(directory,'/',sprintf('%03d',i),'.csv',sep=''))
        
        # remove NA
        data<-data[,pollutant]
        data<-data[!is.na(data)]
        
        # get sum & num of chosen pollutant 
        pollut_sum <- pollut_sum + sum(data)
        pollut_num <- pollut_num + length(data)

    }
    
    pollut_sum/pollut_num
}