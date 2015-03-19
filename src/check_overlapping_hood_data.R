library(dplyr)
library(reshape2)

check_overlapping_hood_data <- function(all_data_summary){
    repeated_hoods <- all_data_summary %>% 
        group_by(hood) %>%
        dplyr::summarize(count=n()) %>%
        filter(count>1)
    for(h in 1:nrow(repeated_hoods)){
        #find files with repeated hoods
        hood <- repeated_hoods$hood[h]
        all_hood_data <- list()    
        for(c in 1:length(all_formatted_data)){
            if(hood %in% names(all_formatted_data[[c]])) {
                all_hood_data[[length(all_hood_data)+1]] <- data.frame(dttm=all_formatted_data[[c]][,c("dttm")],
                                                                       hood=all_formatted_data[[c]][,c(hood)],
                                                                       file=c)
            }
        }
        all_hood_data <- dplyr::rbind_all(all_hood_data)
        repeated_dates <- all_hood_data %>% 
            group_by(dttm) %>%
            dplyr::summarize(count=n()) %>%
            filter(count>1)
        if(nrow(repeated_dates)==0){ next
        }else{
            print(paste(hood,"has overlapping data in files", toString(unique(all_hood_data$file))))
            repeated_data <- all_hood_data %>%
                filter(dttm %in% repeated_dates$dttm) 
            compared_data <- dcast(repeated_data, dttm~file, value.var="hood")
            files <- ncol(compared_data)-1
            if(files==2){ compared_data$diff <- compared_data[,2]- compared_data[,3]
            }else{stop("capability to compare more than 2 files not yet developed")}
            if(sum(compared_data$diff, na.rm=T)>0) warning("difference in data")
        }
    }
    print("not yet capable of combining files on consistent data")
}#function