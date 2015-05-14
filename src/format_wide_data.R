library(reshape2)
library(dplyr)

#given a line of data and an optional minimum number of columns, find the separator
find_separator <- function(data_row, min_cols=2){
  tryCatch({
    
    #find the separator between fields
    tab_separated   <- length(strsplit(data_row, "\t")[[1]])
    
    temp <- gsub("[,]{2,}",",",data_row)       #remove repeated characters
    comma_separated <- length(strsplit(temp, ",")[[1]])
    
    temp            <- gsub("([[:space:]]){2,}",",",data_row)
    space_separated <- length(strsplit(temp, ",")[[1]])
    
    
    if(tab_separated>=min_cols){                      separator <- "\t"       #\t has first priority
    }else if(comma_separated>=min_cols){              separator <- ","     
    }else if(space_separated>=min_cols){              separator <- "([[:space:]]){2,}"
    }else{    stop("separator not found")
    }
  }, error = function(e) print(paste0("Error in find_separator: ", e))
  )#end tryCatch
  return(separator)
}

format_wide_data <- function(original_data, file_format){
  tryCatch({
    
    key_row   <- file_format$p1
    time_row  <- file_format$p2
    date_row  <- file_format$p3
    
    num_hoods <- time_row-1-key_row
    separator <- find_separator(data_row= original_data[date_row], min_cols= num_hoods)
    
    #format hood names
    meta_key <- original_data[(key_row+1):(time_row-1)]
    separator <- find_separator(data_row= meta_key[1], min_cols= 3)
    meta_key <- lapply(meta_key, FUN=function(r){              #keep metadata together
      if(separator!=",")  r <- gsub(separator,",",r)   #replace separators with commas 
      new_line <- strsplit(r,",")
      key <- gsub(":","", (new_line[[1]][1]))
      data.frame(key=tolower(key), hood=new_line[[1]][2], other=toString(new_line[[1]][3: length(new_line[[1]])]))
    })
    meta_key <- dplyr::rbind_all(meta_key)
    
    #format other metadata
    meta <- original_data[(time_row):(date_row-1)]
    separator <- find_separator(data_row= meta[1], min_cols= 2)
    meta <- lapply(meta, FUN=function(r){              #keep metadata together
      if(separator!=",")  r <- gsub(separator,",",r)   #replace separators with commas 
      new_line <- strsplit(r,",")
      data.frame(field=(new_line[[1]][1]), value=new_line[[1]][2])
    })
    meta <- dplyr::rbind_all(meta)
    
    #format hood data
    data <- original_data[date_row:(length(original_data)-1)]    #last row of report does not contain data
    separator <- find_separator(data_row= data[1], min_cols= num_hoods)
    data <- lapply(data, FUN=function(r){    #keep interval data together
      if(separator!=",")  r <- gsub(separator,",",r)   #replace separators with commas 
      new_line <- strsplit(r,",")
      data.frame(t(new_line[[1]]))
    })
    name_row <- as.vector(t(data[[1]]))
    data <- dplyr::rbind_all(data[2:length(data)])
    
    #col names should be a single word
    name_row <- unname(sapply(name_row, FUN=function(n){
      temp            <- gsub("([[:space:]]){2,}",",",n)
      space_separated <- length(strsplit(temp, ",")[[1]])
      if(space_separated==1){ return(temp)
      }else{return(strsplit(temp, ",")[[1]][1])}
    }))
    name_row <- gsub("[:]","",name_row)
    
    #format datetime data
    if(ncol(data)==length(name_row)-1){
      #date/time columns may have been combined to one column, and column names don't match
      names(data) <- c("dttm",name_row[3:length(name_row)])
      
    }else if(ncol(data)==length(name_row)){
      #date/time columns are separate
      date_time_check <- grepl("DATE|Date",name_row[1]) & grepl("Time|TIME",name_row[2])
      if(!all(date_time_check)) warning("check date/time columns in data")
      names(data) <- name_row
      data$dttm <- apply( data[ ,c(1,2)] , 1 , paste , collapse = " " )        
      data[,1] <- NULL
      data[,1] <- NULL
      
    }else{
      warning("number of data columns does not match column names")
    }
    
    data$dttm <- as.POSIXct(data$dttm, format="%m/%d/%Y %H:%M")    #date formatting
    
    if(as.numeric(format(data$dttm[1],"%Y"))<2000){   #date formatted to wrong millenia
      data$dttm <- paste0(as.numeric(substr(as.character(data$dttm),1,4)) +2000, substr(as.character(data$dttm),5,19))
      data$dttm <- as.POSIXct(data$dttm, format="%Y-%m-%d %H:%M")    #date formatting
    }
    
    #relabel sash positions and format numbers
    data_long <- melt(data, id.vars = "dttm") 
    data_long$value[data_long$value=="CLOSED"]    <- 0                                                #number formatting
    data_long$value[data_long$value=="OPEN"]      <- 1                                               #number formatting
    data_long$value[data_long$value=="No Data"]   <- NA                                              #number formatting
    data_long$value[grepl("[a-zA-Z]",data_long$value)]   <- NA                                              #number formatting
    data   <- dcast(data_long, dttm~ variable)  
    data   <- data.frame(dttm=data[,1],
                         apply(data[,2:ncol(data)],2,FUN=as.numeric))      #correct numberformatting 
    
    #name columns with hood names
    data_cols <- data.frame(key=tolower(names(data)))       #some files have inconsistent capitalization between key and data
    data_cols <- dplyr::left_join(data_cols, meta_key, by="key")
    data_cols$hood
    names(data)[!is.na(data_cols$hood)] <- data_cols$hood[!is.na(data_cols$hood)]
  }, error = function(e) print(paste0("Error in find_separator: ", e))
  )#end tryCatch
  return(data)
}