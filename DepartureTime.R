
DepartureTime <- function(method = "H",
                          dy = format(Sys.Date(), "%Y"),  
                          dm = format(Sys.Date(), "%m"), 
                          dd = format(Sys.Date(), "%d"),
                          tmin = 0, tmax = 24,
                          res = 5,
                          MMDD = TRUE,
                          ptw = FALSE,
                          path = getwd(),
                          file = "DepTime"){
  
  if (!requireNamespace("foreign", quietly = TRUE)) {
    stop("Package \"foreign\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  
  # create data.frame 
  Temp <- data.frame(Date = seq(from = as.POSIXct(paste(dy,dm,dd, sep = "-")) + as.difftime(tmin, units="hours"),
                        to = as.POSIXct(paste(dy,dm,dd, sep = "-")) + as.difftime(tmax, units="hours"),
                        by = 60))
  
  # hybrid sampling method
  if(method == "H" | method == "Hybrid") {
      # select departure times
      fixed_rows <- seq(1, nrow(Temp), res)
      random_rows <- numeric(0)
      n <- 1
      for(i in fixed_rows){
        row_max <- i + res - 1
        if(row_max <= nrow(Temp)){ # it limits time-window to those that have a full coverage
          if(ptw == TRUE){print(paste("time window", paste0(n, ":"), "start:", Temp[i,], "end:", Temp[row_max,]))}
        random_rows <- c(random_rows, sample(i:row_max, 1)) } 
        n <- n +1}
      
      Temp <- Temp[random_rows,]
      rm(n, i, row_max, fixed_rows, random_rows)
      }
  
  
  # simple random method
  if(method == "R" |  method == "Random") {
    # random_rows <- sample(1 : nrow(Temp), nrow(Temp)/res)
    Temp <- sort(Temp[sample(1 : nrow(Temp), nrow(Temp)/res), ])
  } 
  
  # systematic method
  if(method == "S" |  method == "Systematic") {
    Temp <- Temp[seq(1, nrow(Temp), res),]
  } 
  
  # Constrained Random Walk Sampling
  if(method == "W" |  method == "ConstrainedWalk") {
    random_rows <- sample(1:(res), 1)   # select first departure time
    
    while(tail(random_rows, n=1) <= nrow(Temp)) {
      random_rows <- c(random_rows, 
                       sample((round(tail(random_rows, n=1) + res*0.5, 0)):
                                (round(tail(random_rows, n=1) + res*1.5, 0)), 1) ) }
    random_rows <- random_rows[-length(random_rows)] # remove departure time out of time-window
    Temp <- Temp[random_rows,]
  }
  
  if(MMDD == TRUE){
    Temp <- data.frame(ID = seq(0, length(Temp)-1),
                       Date = paste(paste(substr(Temp, 6,7), substr(Temp, 9, 10), substr(Temp, 1,4), sep = "/"),
                                    substr(Temp, 12,16), sep = "  ") ) } else {
      Temp <- data.frame(ID = seq(0, length(Temp)-1),
                         Date = paste(paste(substr(Temp, 9, 10), substr(Temp, 6,7), substr(Temp, 1,4), sep = "/"),
                                      substr(Temp, 12,16), sep = "  ") ) }
  # save output
  foreign::write.dbf(Temp, paste(path, paste0(file, ".dbf"), sep = "/"))
  
}





























