format_long_data <- function(original_data){

    original_data <- gsub("\"", "",original_data)                                      #remove unnecessary quotes within lines
    start_chunks <- c(which(grepl("Point",original_data)),(length(original_data)+1))   #get line at which chunk starts
    num_chunks   <- sum(grepl("Point",original_data))                          #count number of chunks
    
    #reformat data: break file containing many chunks into single list 
    all_data <- list()
    for(c in 1:num_chunks){
        chunk_data <- original_data[start_chunks[c]:(start_chunks[c+1]-1)]    #isolate data within a chunk
        
        #format metadata
        meta <- chunk_data[grepl("^[0-9]", chunk_data)==FALSE]
        meta <- lapply(meta, FUN=function(r){              #keep metadata together
            new_line <- strsplit(r,",")
            data.frame(field=(new_line[[1]][1]), value=new_line[[1]][2])
        })
        meta <- dplyr::rbind_all(meta)
        
        #format hood data
        data <- chunk_data[grepl("^[0-9]", chunk_data)==TRUE]
        data <- lapply(data, FUN=function(r){    #keep interval data together
            new_line <- strsplit(r,",")
            data.frame(date=new_line[[1]][1], 
                       time=new_line[[1]][2],
                       cfm=new_line[[1]][3],
                       value=new_line[[1]][4])
        })
        data <- dplyr::rbind_all(data)
        
        #data <- data[!is.na(data$cfm),]                                                  #remove filler rows
        data$dttm <- as.POSIXct(paste(data$date, data$time), format="%m/%d/%Y %H:%M")    #date formatting
        data$cfm <- as.numeric(data$cfm)                                                 #number formatting
        
        all_data[[c]] <- list(meta=meta, data=data)
    }
    
    #reformat data into wide format
    for(c in 1:length(all_data)){
        hood <- all_data[[c]]$meta$value[1]
        data <- all_data[[c]]$data[,c("dttm","cfm")]
        names(data)[2] <- hood
        if(c==1){      formatted_data <- data
        }else{         formatted_data <- plyr::join(formatted_data,data, by="dttm", type="full")
        }  
    }
    
    #remove all columns that end with STPT?
    return(formatted_data)
}