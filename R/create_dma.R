create_dma <- function(x,target,baselines,align){ 
  data <- x # rename input data

    # Initialize and name data columns for moving average time series
    target.name <- paste0("DMA",target) # initialize a vector for the target time series ("("smoothed actual volume of coverage")
    baseline.names <- paste0("DMA",baselines) # initialize a vector for the various baseline comparison time series ("normal/expected volume of coverage")
    target.name.s <- paste0("DMA",target,".share") # Target time series as shares rather than absolute values
    baseline.names.s <- paste0("DMA",baselines,".share") # Baseline comparison time series as shares rather than absolute values 
    target.name.a <- paste0("a",target)
    baseline.names.a <- paste0("a",baselines)

    # Set counter
    first <- 1 # start day count
    last <- dim(data)[1] # end day count
 
    # Set all initial values to zero; are filled with data by the rollmean procedure
    ## In target time series
    data[,target.name] <- 0
    data[,target.name.s] <- 0
    data[,target.name.a] <- 0 # This is an empty column for later recording whether a news wave is above this threshold or not.

    ## In baseline time series
    for (j in 1:length(baseline.names))
      {
      data[,baseline.names[j]] <- 0
      data[,baseline.names.s[j]] <- 0
      data[,baseline.names.a[j]] <- 0 # This is an empty column for later recording whether a news wave is above this threshold or not.
      }

    # Calculate rolling means for target time series
    data[,target.name] <- rollmean(k=target,x=data[,"count0"],align=align,fill=c(NA,NA,NA))
    data[,target.name.s] <- rollmean(k=target,x=data[,"share0"],align=align,fill=c(NA,NA,NA))
    
    ## Right-aligned rolling means
    if (align=="right")
      {
      data[,target.name][first:(target-1)] <- data[,target.name][target]
      data[,target.name.s][first:(target-1)] <- data[,target.name.s][target]
      }

    ## Left-aligned rolling means
    if (align=="left")
      {
      data[,target.name][(last-target+2):last] <- data[,target.name][last-target+1]
      data[,target.name.s][(last-target+2):last] <- data[,target.name.s][last-target+1]
      }

    ## Center-aligned rolling means
    if (align=="center")
      {
      data[,target.name][1:(round(target/2))] <- data[,target.name][round(target/2)+1]
      data[,target.name.s][1:round(target/2)] <- data[,target.name.s][round(target/2)+1]
      data[,target.name][(last-round(target/2)+1):last] <- data[,target.name][(last-round(target/2))]
      data[,target.name.s][(last-round(target/2)+1):last] <- data[,target.name.s][(last-round(target/2))]
      }				

    # Calculate rolling means for baseline comparison time series
    for (j in 1:length(baseline.names))
      {
      data[,baseline.names[j]] <- rollmean(k=baselines[j],x=data[,"count0"],align=align,fill=c(NA,NA,NA))
      data[,baseline.names.s[j]] <- rollmean(k=baselines[j],x=data[,"share0"],align=align,fill=c(NA,NA,NA))

      ## Right-aligned rolling means
      if (align=="right")
        {
        data[,baseline.names[j]][first:(baselines[j]-1)] <- data[,baseline.names[j]][baselines[j]]
        data[,baseline.names.s[j]][first:(baselines[j]-1)] <- data[,baseline.names.s[j]][baselines[j]]			
        }

      ## Left-aligned rolling means
      if (align=="left")
        {
        data[,baseline.names[j]][last-baselines[j]+1:last] <- data[,baseline.names[j]][last-baselines[j]]
        data[,baseline.names.s[j]][last-baselines[j]+1:last] <- data[,baseline.names.s[j]][last-baselines[j]]			
        }	

      ## Center-aligned rolling means
      if (align=="center")
        {
        data[,baseline.names[j]][first:(round(baselines[j]/2))] <- data[,baseline.names[j]][round(baselines[j]/2)+1]
        data[,baseline.names.s[j]][first:(round(baselines[j]/2))] <- data[,baseline.names.s[j]][round(baselines[j]/2)+1]			
        data[,baseline.names[j]][(last-round(baselines[j]/2)+1):last] <- data[,baseline.names[j]][(last-round(baselines[j]/2))]
        data[,baseline.names.s[j]][(last-round(baselines[j]/2)+1):last] <- data[,baseline.names.s[j]][(last-round(baselines[j]/2))]
        }	
      }

  return(data)	    # Return updated data.frame
}

