calc_plot_hood_metrics <- function(compressed_data_files, compressed_all_formatted_data, compressed_file_summary, wd_output){
    
    #begin pdf creation for all time series graphs
    pdf(paste0(wd_output,"fume_hood_time_series.pdf"), width=12, height=4)
    
    #calculate metrics and save to file
    all_hood_summary <- calculate_hood_metrics(all_formatted_data=compressed_all_formatted_data, 
                                               data_files=compressed_data_files,
                                               file_summary=compressed_file_summary, 
                                               plot_ts=T)
    
    #combine with file summary data and save to file
    all_hood_summary <- dplyr::left_join(compressed_file_summary, all_hood_summary, by="hood")
    write.csv(all_hood_summary, file=paste0(wd_output,"hood_summary.csv"), row.names=FALSE)
    
    # end pdf creation
    graphics.off()         
    closeAllConnections()
    return(all_hood_summary)
}