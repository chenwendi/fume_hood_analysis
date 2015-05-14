reduce_timeframe_hood_data <- function(all_data_summary, all_formatted_data, start="2015-03-01", end="2015-03-08"){
  tryCatch({
    
    if(!"list" %in% class(all_formatted_data))     stop("all_formatted_data must be a data.frame")
    ####all_data_summary isn't really needed
    
    startdttm <- as.POSIXct(start)
    enddttm <- as.POSIXct(end)
    
    reduced_formatted_data <- list()
    for(f in 1:length(all_formatted_data)){
      f_data                      <- all_formatted_data[[f]]
      f_data_limited              <- f_data[f_data$dttm>=startdttm & f_data$dttm<enddttm,]
      reduced_formatted_data[[f]] <- f_data_limited
      
    }
  }, error = function(e) print(paste0("Error in reduce_timeframe_hood_data: ", e))
  )#end tryCatch
  return(reduced_formatted_data)
}