complete<-function(directory, id=1:332)
{
    pollut_num <- numeric(0)
    
    for (i in id)
    {
        # read data from .csv
        data<-read.csv(paste(directory,'/',sprintf('%03d',i),'.csv',sep=''))
        
        # remove NA
        data<-data[complete.cases(data),]
        
        # get nobs
        pollut_num <- c(pollut_num, nrow(data))
    }
    
    data.frame(id=id, nobs=pollut_num)
}