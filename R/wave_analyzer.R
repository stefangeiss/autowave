# *x* is a list of time series data with a variable called "a.pnw.no" , that counts the news waves in the time series, as created by the find_newswaves function, and a variable "day" that counts the days in the time series continuously. In addition, "n90", "n180", "n365", "n730", "n1825", "day", "DMA30", "DMA90", "DMA180", "DMA365", "DMA730", "DMA1825", and "topic" should be included. 

  wave_analyzer <- function(x){

    nx <- names(x)[str_detect(string=names(x),pattern="^n[:digit:]+")] # Retrieves the column names of all "n" elements of the time series (absolute numbers above baselines)
    DMAx <- names(x)[str_detect(string=names(x),pattern="^DMA[:digit:]+")] # Retrieves the smoothed time series names
    nums <- str_extract(DMAx,"[:digit:]*$") # Retrieves all the smoothing intervals

    data <- x[,c("a.pnw.no","day","topic",nx, DMAx)] # Creates a new data.frame with only the relevant information

    minwaves <- min(data$a.pnw.no,na.rm=T) # ID number of the first news wave in the data.frame
    maxwaves <- max(data$a.pnw.no,na.rm=T) # ID number of the last news wave in the data.frame

    wavedesc <- data.frame(id=NA,volume=NA,duration=NA,intensity=NA,start=NA,end=NA) # A new data.frame in which the descriptive statistics, aggregations for each news wave are recorded.

    for (w in minwaves:maxwaves) # a loop repeated for each of the news waves in "data"
    {
      wave <- subset(data,a.pnw.no==w)
  #		waveend <- subset(wave,o30dur==max(wave$o30dur,na.rm=TRUE))
      wavedesc[w-minwaves+1,"id"] <- w # record the wave's ID number
      wavedesc[w-minwaves+1,"duration"] <- dim(wave)[1] # Record the length of the news wave in days.
      wavedesc[w-minwaves+1,"volume"] <- mean(colSums(as.matrix(wave[,nx]),na.rm=TRUE),na.rm=TRUE) # Record the total volume of the news wave. We first calculate the sum of above-baseline coverage, per baseline, and then average across baselines)
      wavedesc[w-minwaves+1,"intensity"] <- mean(colMeans(as.matrix(wave[,nx]),na.rm=TRUE),na.rm=TRUE) # Record the daily intensity of the news wave. We first calculate the mean of above-baseline coverage, per baseline, and then average across baselines)
      wavedesc[w-minwaves+1,"max.intensity"] <- mean(colMaxs(as.matrix(wave[,nx]),na.rm=TRUE),na.rm=TRUE) # Record the maximum intensity of the news wave. We first compute the maximum above-baseline values per baseline, and then average across baseline.  
      wavedesc[w-minwaves+1,"variability"] <- mean(colSds(as.matrix(wave[,nx]),na.rm=TRUE),na.rm=TRUE) # Record the variability of coverage during the news wave. We first compute the standard deviation of above-baseline coverage intensity (per baseline). Then we average across baselines.
      wavedesc[w-minwaves+1,"start"] <- wave$day[1] # Record the day the news wave starts.
      wavedesc[w-minwaves+1,"end"] <- wave$day[length(wave$day)] # Record the day the news wave ends.

      # Record the different baselines
      for (d in 1:length(DMAx)){
        varname <- paste0("baseline",nums[d])
        wavedesc[w-minwaves+1,varname] <- mean(wave[,DMAx[d]],na.rm=TRUE)
      }

      wavedesc[w-minwaves+1,"topic250"] <- wave$topic[1] # Record the topic the news wave belongs to or was found in

      if (w%in%thousands) print(w) # A progress counter: how many topics have been processed?
      flush.console()
    }
    return(wavedesc) # Return data.frame with data about the news waves.
  }