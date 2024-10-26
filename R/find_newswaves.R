
find_newswaves <- function(x,target,baselines,critical.baselines)
    {
    data <- x # rename input to "data"

    # Rename columns in case they have a different name than expected
    target.name <- paste0("DMA",target)	
    target.name.s <- paste0("DMA",target,".share")	
    target.name.a <- paste0("a",target)	
    baseline.names <- paste0("DMA",baselines)
    baseline.names.s <- paste0("DMA",baselines,".share")
    baseline.names.a <- paste0("a",baselines)
    baseline.names.v <- paste0("v",baselines)
    baseline.names.n <- paste0("n",baselines)

    # Check for each baseline whether the target time series is "above" the baseline
    for (j in 1:length(baselines))
      {
      data[,baseline.names.a[j]] <- NA
      data[,baseline.names.a[j]] <- 1*(data[,target.name] > data[,baseline.names[j]])
      }
 
    # Sum up across all baselines: How many of the baselines are "beaten" by the smoothed current value 
    data[,"a"] <- rowSums(data[,baseline.names.a])

    # We lag the sum by 1 day
    data[,"a.lag1"] <- c(NA,data[,"a"][-(length(data$a)-1)])

    # Calculate the above-baseline volume of coverage: 
    # How much does coverage on a certain day exceed the expected volume according to all the different baselines.
    # This is important for later calculating volume, intensity, and maxima of news waves.
    for (j in 1:length(baselines))
      {
      # all variables with "v" relate to shares (in target and baseline)
      data[,baseline.names.v[j]] <- NA 
      # all variables with "n" relate to absolute counts (in target and baselines)
      data[,baseline.names.n[j]] <- NA
      # Shares: If a news wave is active, calculate how much the target is above the baseline(s)
      data[,baseline.names.v[j]] <- (data[,baseline.names.a[j]]) * (data[,target.name.s]-data[,baseline.names.s[j]]) 
      # Absolute numbers: If a news wave is active, calculate how much the target is above the baseline(s)
      data[,baseline.names.n[j]] <- (data[,baseline.names.a[j]]) * (data[,target.name]-data[,baseline.names[j]]) 
      }

    # A market that a new time series on a new topic is starting. 
    # Allows to disrupt any time series calculation or split the data before each new topic. 
    # If doing the procedure listwise by topic (as in the demo), this is not necessary.
    data$new.topic <- 0
    data$new.topic[seq(1,length(data$new.topic),86197)] <- 1

    # Record the days on which news coverage first surpasses the critical number of baselines 
    # for starting a new potential news wave (turn.on) and first drops below the critical number 
    # of baselines again, ending the current potential newswave (turn.off)
    data$turn.on <- 1*(data$a.lag1<critical.baselines & data$a>(critical.baselines-1))
    data$turn.off <- 1*(data$a<critical.baselines & data$a.lag1>(critical.baselines-1))

    # Record whether on a given day there is an active potential newswave regarding the topic.
    data$pnw <- (data$turn.on==1 | data$a>(critical.baselines-1))

    # Give ID numbers of each of the potential news waves. 
    # This is a counter that increases by one each time a new potential newswave starting point is detected.
    data$pnw.no <- cumsum(data$turn.on)

    # Restrict the ID numbers of periods in which there really is an active potential newswave
    data$a.pnw.no <- (data$a>(critical.baselines-1))*data$pnw.no

    return(data) # return the modified data fram
    }
