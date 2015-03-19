library(reshape2)
library(dplyr)

format_wide_data <- function(original_data, file_format){
    key_row   <- file_format$p1
    time_row  <- file_format$p2
    date_row  <- file_format$p3
    
    #find the separator between fields
    tab_separated   <- length(strsplit(original_data[date_row], "\t")[[1]])
    comma_separated <- length(strsplit(original_data[date_row], ",")[[1]])

    temp            <- gsub("([[:space:]]){2,}",",",original_data[date_row])
    space_separated <- length(strsplit(temp, ",")[[1]])
    num_hoods <- time_row-1-key_row
    
    if(tab_separated>num_hoods){                      separator <- "\t"       #\t has first priority
    }else if(comma_separated>num_hoods){              separator <- ","     
    }else if(space_separated>num_hoods){              separator <- "([[:space:]]){2,}"
    }else{    stop("separator not found")
    }
    
    #format hood names
    meta_key <- original_data[(key_row+1):(time_row-1)]
    meta_key <- lapply(meta_key, FUN=function(r){              #keep metadata together
        if(separator!=",")  r <- gsub(separator,",",r)   #replace separators with commas 
        new_line <- strsplit(r,",")
        key <- gsub(":","", (new_line[[1]][1]))
        data.frame(key=tolower(key), hood=new_line[[1]][2], other=toString(new_line[[1]][3: length(new_line[[1]])]))
    })
    meta_key <- dplyr::rbind_all(meta_key)
    
    #format other metadata
    meta <- original_data[(time_row):(date_row-1)]
    meta <- lapply(meta, FUN=function(r){              #keep metadata together
        if(separator!=",")  r <- gsub(separator,",",r)   #replace separators with commas 
        new_line <- strsplit(r,",")
        data.frame(field=(new_line[[1]][1]), value=new_line[[1]][2])
    })
    meta <- dplyr::rbind_all(meta)

    #format hood data
    data <- original_data[date_row:(length(original_data)-1)]    #last row of report does not contain data
    data <- lapply(data, FUN=function(r){    #keep interval data together
        if(separator!=",")  r <- gsub(separator,",",r)   #replace separators with commas 
        new_line <- strsplit(r,",")
        data.frame(t(new_line[[1]]))
    })
    name_row <- data[[1]]
    data <- dplyr::rbind_all(data[2:length(data)])
    
    #format datetime data
    if(ncol(data)==length(name_row)-1){
        #date/time columns may have been combined to one column, and column names don't match
        names(data) <- c("dttm",name_row[3:length(name_row)])
    
    }else if(ncol(data)==length(name_row)){
        #date/time columns are separate
        date_time_check <- grepl("DATE|Date",name_row[1]) & grepl("Time|TIME",name_row[2])
        if(!all(date_time_check)) warning("check date/time columns in data")
        names(data) <- name_row
        data$dttm <- paste(data[,1], data[,2])
        data[,1] <- NULL
        data[,1] <- NULL
        
    }else{
        warning("number of data columns does not match column names")
    }
    
    data$dttm <- as.POSIXct(data$dttm, format="%m/%d/%Y %H:%M")    #date formatting

    #relabel sash positions and format numbers
    data_long <- melt(data, id.vars = "dttm")    
    data_long$value[data_long$value=="CLOSED"]    <- 0                                                #number formatting
    data_long$value[data_long$value=="OPEN"]      <- 1                                               #number formatting
    data_long$value[data_long$value=="No Data"]   <- NA                                              #number formatting
    data   <- dcast(data_long, dttm~ variable)  
    data   <- data.frame(dttm=data[,1],
                         apply(data[,2:ncol(data)],2,FUN=as.numeric))      #correct numberformatting 
    
    #name columns with hood names
    data_cols <- data.frame(key=tolower(names(data)))       #some files have inconsistent capitalization between key and data
    data_cols <- dplyr::left_join(data_cols, meta_key, by="key")
    data_cols$hood
    names(data)[!is.na(data_cols$hood)] <- data_cols$hood[!is.na(data_cols$hood)]
    
    return(data)
    }