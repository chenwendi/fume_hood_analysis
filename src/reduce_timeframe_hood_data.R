reduce_timeframe_hood_data <- function(all_data_summary, all_formatted_data, start="2015-03-01", end="2015-03-08"){
    if(!"list" %in% class(all_formatted_data))     stop("all_formatted_data must be a data.frame")
    if(!"data.frame" %in% class(all_file_summary)) stop("all_file_summary must be a data.frame")
    
    startdttm <- as.POSIXct(start)
    enddttm <- as.POSIXct(end)
    
    reduced_formatted_data <- list()
    for(f in 1:length(all_formatted_data)){
        f_data                      <- all_formatted_data[[f]]
        f_data_limited              <- f_data[f_data$dttm>=startdttm & f_data$dttm<enddttm,]
        reduced_formatted_data[[f]] <- f_data_limited
        
    }
    return(reduced_formatted_data)
}