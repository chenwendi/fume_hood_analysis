reduce_unnecessary_hood_data <- function(all_data_summary, all_formatted_data){
    if(!"list" %in% class(all_formatted_data))     stop("all_formatted_data must be a data.frame")
    if(!"data.frame" %in% class(all_file_summary)) stop("all_file_summary must be a data.frame")

    if("exclude" %in% names(all_file_summary))     
        all_data_summary <- all_data_summary[all_data_summary$exclude==FALSE,]
    
    reduced_formatted_data <- list()
    for(f in 1:length(all_formatted_data)){
        f_data                      <- all_formatted_data[[f]]
        orig_hoods                  <- setdiff(names(f_data),"dttm")
        hoods_in_file               <- orig_hoods[orig_hoods %in% all_data_summary$hood]
        reduced_formatted_data[[f]] <- f_data[,c("dttm",hoods_in_file)]
    }
    return(reduced_formatted_data)
}
    