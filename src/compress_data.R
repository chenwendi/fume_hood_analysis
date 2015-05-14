# Summarize all files, list hoods within them, compress datasets, resummarize data

compress_data <- function(data_files, all_formatted_data, hood_mapping, wd_output){
  #some basic checks
  if(length(data_files)==0)         stop("data_files is empty")
  if(length(all_formatted_data)==0) stop("all_formatted_data is empty")
  if(!is.data.frame(hood_mapping))  stop("hood_mapping is invalid input type")
  if(nrow(hood_mapping)==0)         stop("hood_mapping is empty")
  if(!exists("wd_output"))          stop("wd_output is empty")
  
  tryCatch({
    
    #calculate metrics for initial data, format to include hood data and exclude nonrelevant hoods
    all_file_summary <- calculate_file_metrics(all_formatted_data, data_files)
    all_file_summary <- dplyr::left_join(all_file_summary,hood_mapping, by="hood")
    all_file_summary <- all_file_summary[!is.na(all_file_summary$exclude) & all_file_summary$exclude==FALSE,]
    select_hoods_formatted_data <- reduce_unnecessary_hood_data(all_file_summary, all_formatted_data)      #eliminates excluded hoods from data
    write.csv(all_file_summary, file=paste0(wd_output,"data_summary_initial.csv"), row.names=F)
    
    # plot data availability
    pdf(paste0(wd_output,"data_availability_initial.pdf"), width=12, height=4)
    plot_data_availability(all_data_summary=all_file_summary)
    graphics.off(); closeAllConnections()
    
    # compare files with oveelapping data for consistency and compress datasets
    results                     <- check_merge_hood_data(all_file_summary, all_formatted_data=select_hoods_formatted_data, data_files) 
    compressed_data_files         <- results$data_files_new   #old object: data_files
    compressed_all_formatted_data <- results$all_data_new     #old object: all_formatted_data
    
    #calculate metrics for compressed data, format to include hood data and exclude nonrelevant hoods
    all_file_summary <- calculate_file_metrics(compressed_all_formatted_data, compressed_data_files)   #recalc for combined files
    all_file_summary <- dplyr::left_join(all_file_summary,hood_mapping, by="hood")
    all_file_summary <- all_file_summary[all_file_summary$exclude==FALSE,]
    write.csv(all_file_summary, file=paste0(wd_output,"data_summary_compressed.csv"), row.names=F)
    
    #check there are no irrelevant files
    if(!all(compressed_data_files %in% unique(all_file_summary$file))) 
      stop(paste("no relevant data in file", unique(compressed_data_files[!compressed_data_files %in% unique(all_file_summary$file)])))
    
    pdf(paste0(wd_output,"data_availability_compressed.pdf"), width=12, height=4)
    plot_data_availability(all_file_summary)
    graphics.off(); closeAllConnections()
    
    # check: confirm all hood names are listed in hood_mapping
    all_hood_names <- all_file_summary$hood
    unmapped_hoods <- all_hood_names[!all_hood_names %in% hood_mapping$hood]
    if(length(unmapped_hoods)>0) stop("there are unmapped hoods")
    
  }, error = function(e) print(paste0("Error in compress_data: ", e))
  )#end tryCatch
  
  return(list(compressed_data_files=compressed_data_files,compressed_all_formatted_data=compressed_all_formatted_data, compressed_file_summary=all_file_summary))
  
}

