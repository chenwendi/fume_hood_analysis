library(dplyr)
library(reshape2)

#this script makes the following assumptions (and may need to be changed if these assumptions are wrong):
#   2 datafiles with overlapping hood data will contain data for all the same hoods, there are no files that contain data for only a fraction of the relevant hoods

check_merge_hood_data <- function(all_file_summary, all_formatted_data, data_files){
  tryCatch({
    
    if(!is.data.frame(all_file_summary)) stop("all_file_summary must be a data.frame")
    if(!"list" %in% class(all_formatted_data))     stop("all_formatted_data must be a data.frame")
    if(nrow(all_file_summary)==0)                  stop("all_file_summary has no data")
    
    #determine which hoods have repeated intervals
    repeated_hoods <- all_file_summary %>% 
      group_by(hood) %>%
      dplyr::summarize(count=n()) %>%
      filter(count>1)
    
    repeated_data <- all_file_summary[all_file_summary$hood %in% repeated_hoods$hood,]
    
    #determine which files have overlapping hood data
    file_matches <- list()
    for(r in 1:nrow(repeated_data)){
      matches <- repeated_data$file[repeated_data$hood==repeated_data$hood[r]]
      matches <- toString(matches)
      if(matches %in% file_matches){  #do nothing
      }else{         file_matches[[length(file_matches)+1]] <- matches
      }
    }
    
    #loop through each set of matching files and combine across dates
    data_files_subtract <- data_files
    data_files_new <- c()    # contains aggregated filenames
    all_data_new <- list()   # contains aggregated data
    for(f in 1:length(file_matches)){
      print(f)
      matches <- strsplit(file_matches[[f]], split=", ")[[1]]
      
      if(length(matches)==0) next 
      
      f1 <- all_formatted_data[[which(data_files==matches[1])]]
      f2 <- all_formatted_data[[which(data_files==matches[2])]]
      
      f12 <- combine_2_files(f_a=f1, f_b=f2)
      
      if(length(matches)==2){          
        f_final <- f12
      }else{
        num_loops <- length(matches)-2
        
        for(it in 1:num_loops){
          print(it)
          f3 <- all_formatted_data[[which(data_files==matches[it+2])]]
          f12 <- combine_2_files(f12, f3)
          
          if(it==num_loops)   f_final <- f12
        }
        
      }
      
      #update data file and names of files for subsequent analysis
      data_files_subtract <- setdiff(data_files_subtract, matches)
      data_files_new[f] <- toString(gsub(".csv","",matches))
      all_data_new[[f]] <- f_final
    }#for
    
    #add back the single hood files (non-combined files)
    data_to_add <- which(data_files %in% data_files_subtract)
    for(f in data_to_add){
      #         print(data_files[f])
      data_files_new[length(data_files_new)+1] <- data_files[f]
      all_data_new[[length(all_data_new)+1]]   <- all_formatted_data[[f]]
    }
  }, error = function(e) print(paste0("Error in check_merge_hood_data: ", e))
  )#end tryCatch
  return(list(data_files_new=data_files_new, all_data_new=all_data_new))   
}#function



combine_2_files <- function(f_a, f_b){
    tryCatch({
        
        #determine which timestamps have overlapping data
        overlapping_dates <- f_a$dttm[f_a$dttm %in% f_b$dttm]
        overlapping_dates <- overlapping_dates[order(overlapping_dates)]      #summary(overlapping_dates)
        
        if(length(overlapping_dates)==0){    #if no overlapping dates, combine files w rbind
            f_c <- rbind(f_a, f_b)
            dim_check_1 <- dim(f_a)[1]+dim(f_b)[1]==dim(f_c)[1]
            dim_check_2 <- dim(f_a)[2]==dim(f_b)[2] & dim(f_b)[2]==dim(f_c)[2]
            if(dim_check_1 & dim_check_2){ 
                return(f_c[order(f_c$dttm),])
            }else{stop(paste("error combining data for match",f))}
        }else{    
            #there are overlapping dates, check if they conflict
            f_c <- plyr::rbind.fill(f_a, f_b)
            f_c <- f_c[!duplicated(f_c),]
            rownames(f_c) <- 1:nrow(f_c)
            
            row_differences <- f_c %>% group_by(dttm) %>% dplyr::summarize(count=n()) %>% filter(count>1)
            
            if(nrow(row_differences)==0){   #if there is no conflict, we're fine
                return(f_c[order(f_c$dttm),])
            }else{                          
                #there is conflicting data to overcome
                conflicting_data <- f_c[f_c$dttm %in% row_differences$dttm,]
                conflicting_data <- conflicting_data[order(conflicting_data$dttm),]
                
                #look at conflicting timestamps
                num_comparisons <- nrow(conflicting_data)/2
                rows_to_remove <- c()
                for(pair in 1:num_comparisons){
                    if(TRUE %in% is.na(unique(unlist(conflicting_data[(2*pair-1):(2*pair),])))){      #na values not the same, remove the row with more NA values
                        if(sum(is.na(conflicting_data[(2*pair-1),]))<sum(is.na(conflicting_data[(2*pair),]))){ 
                            rows_to_remove <- c(rows_to_remove, rownames(conflicting_data[(2*pair),]))
                        }else{rows_to_remove <- c(rows_to_remove, rownames(conflicting_data[(2*pair-1),]))} 
                    }
                }
                f_c <- f_c[!rownames(f_c)%in%rows_to_remove,]
                return(f_c[order(f_c$dttm),])
                
                #check again
                row_differences <- f_c %>% group_by(dttm) %>% dplyr::summarize(count=n()) %>% filter(count>1)
                if(nrow(row_differences)>0)            stop(paste("cannot combine conflicting data",f))}
        }
    }, error=function(e){
        print(paste("Error in function 'combine_2_files',", e))
    })
}#function