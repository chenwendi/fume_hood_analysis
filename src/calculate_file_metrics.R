calculate_file_metrics <- function(all_formatted_data, data_files){
    # begin to loop through each set of fume hood data (each set corresponds to a csv file)
    all_file_summary <- lapply(1:length(all_formatted_data), FUN=function(c){
        print(c)
        ##### basic calculations, summary of the data included in a given file
        data_c <- all_formatted_data[[c]]

        fume_hood_names <- setdiff(names(data_c),"dttm") #get names of fume hoods
        
        file_summary           <- data.frame(hood=fume_hood_names)
        file_summary$file      <- data_files[c]
        file_summary$min_date  <-  min(data_c$dttm)
        file_summary$max_date  <- max(data_c$dttm)
        file_summary$days      <- round(file_summary$max_date-file_summary$min_date,2)
        
        return(file_summary)
    })
    all_file_summary <- dplyr::rbind_all(all_file_summary)
    
    return(all_file_summary)
}