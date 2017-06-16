corr<-function(directory, threshold=0)
{
    cor_result_return <- numeric(0)
    
    for (i in 1:332)
    {
        # read data from .csv
        data<-read.csv(paste(directory,'/',sprintf('%03d',i),'.csv',sep=''))
        
        # remove NA
        data<-data[complete.cases(data),]
        
        # if nrow> threshold, get cor result and save it
        if (nrow(data)> threshold)
        {
            cor_result_return <- c(cor_result_return, cor(data["sulfate"], data["nitrate"]))
        }
    }
    
    cor_result_return
}