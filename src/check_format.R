#function checks data for indicators of formatting, returns format type
check_format <- function(original_data){
    key_row   <- c(which(grepl("^Key",original_data)))    
    time_row  <- c(which(grepl("^Time",original_data)))    
    date_row  <- c(which(grepl("^<>Date",original_data)))    
    pointsystem_row  <- c(which(grepl("Point System Name",original_data)))    
    
    if(length(key_row)==1&length(time_row)==1&length(date_row)==1){ 
        return("wide_date format")
    }else if(length(pointsystem_row)>0){
        return("long_date format")
    }else{
        return("format not found")
    }
}