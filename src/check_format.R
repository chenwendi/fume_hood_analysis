#function checks data for indicators of formatting, returns format type
check_format <- function(original_data){
    key_row   <- c(which(grepl("^Key",original_data)))    
    time_row  <- c(which(grepl("^Time",original_data)))    
    date_row  <- c(which(grepl("Date|DATE",original_data)))    
    pointsystem_row  <- c(which(grepl("Point System Name",original_data)))    
    
    #wide format has 2 slight variations, specified in parameters
    if(length(key_row)==1&length(time_row)==1&length(date_row)>=1){ 
        return(list(type="wide_date format", p1=key_row, p2=time_row, p3=max(date_row)))
    }else if(length(time_row)==1&length(date_row)>=1){ 
        return(list(type="wide_date format", p1=0, p2=time_row, p3=max(date_row)))
    
    }else if(length(pointsystem_row)>0){
        return(list(type="long_date format"))
    }else{
        return(list(type="format not found"))
    }
}