# extract, transform, and load data

etl <- function(data_files){
  if(length(data_files)==0) stop("Error: No data files found")
    all_formatted_data <- list()
    for(file in 1:length(data_files)){
        tryCatch({
            print(file)
            original_data <- readLines(paste0(wd_data, data_files[file])) #read lines from csv file
            original_data <- gsub("\"","",original_data)                  #get rid of random quotation marks
            
            ## determine file formatting
            file_format <- check_format(original_data)    #check formatting of file
            
            ## choose formatting function based on current file formatting
            if(file_format$type=="wide_date format"){          formatted_data <- format_wide_data(original_data, file_format)
            }else if(file_format$type=="long_date format"){    formatted_data <- format_long_data(original_data, file_format)
            }else{                                        warning(paste(data_files[file],": data not in recognizable format"))
            }    
            
            all_formatted_data[[file]]<- formatted_data
        },error=function(e){ print(paste(file, data_files[file], e))
        })
    }
    return(all_formatted_data)
}