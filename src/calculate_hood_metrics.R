calculate_hood_metrics <- function(all_formatted_data, data_files, file_summary, plot_ts=TRUE){
    tryCatch({
    # begin to loop through each set of fume hood data (each set corresponds to a csv file)
    all_data_summary <- lapply(1:length(all_formatted_data), FUN=function(c){
        print(c)
        ##### basic calculations, summary of the data included in a given file
        data_c <- all_formatted_data[[c]]
        
        file      <- data_files[c]        
        fume_hood_names <- unique(file_summary$hood[file_summary$file==file])
        
        check_hoods <- fume_hood_names %in% names(data_c)
        if(FALSE %in% check_hoods) stop("hood data not found in accompanying file")
        
        #### summary of individual fume hoods
        hood_summary <- lapply(fume_hood_names, FUN=function(h){
            print(h)
            c_index  <- which(names(data_c)==h)    #column index
            h_data   <- data.frame(dttm=data_c[,"dttm"], hood=data_c[,c_index])

            max_v    <- max(h_data$hood, na.rm=T)
            min_v    <- min(h_data$hood, na.rm=T)
            median_v <- median(h_data$hood, na.rm=T)
            mean_v   <- round(mean(h_data$hood, na.rm=T),3)
            
            #set threshold based on max_v
            threshold <- if(max_v <0){             0     #if data is negative, set threshold to 0
            }else if(max_v<=1){     0.5
            }else if(max_v<=150){   0.5*max_v     #this one is a guess
            }else{                  125 + quantile(h_data$hood, 0.05, na.rm=TRUE)   
            }
            if(threshold>max_v & max_v>0){ threshold <- max_v}
            
            #assign sash state to cfm values
            h_data$sash <- "no data"
            h_data$sash[h_data$hood>=threshold] <- "open"
            h_data$sash[h_data$hood<threshold]  <- "closed"
            
            #missing data: this metric is the only one that directly indicates data completeness
            pct_na     <- round(sum(h_data$sash=="no data")/length(h_data$sash),3)     
            
            #remove na rows...these render all calcs useless for datasets with irregular intervals between hoods
            h_data     <- h_data[h_data$sash != "no data",]
            if(nrow(h_data)==0){
                interval <- NA
                pct_open <- NA
                pct_closed <- NA
                pct_open_wd <- NA
                pct_open_we <- NA
                med_opening_hrs <- 0
                max_opening_hrs <- 0
                openings_over_5 <- 0
                openings_over_8 <- 0
                openings_over_24 <- 0
                pct_exceeding_5 <- 0
            }
                
            intervals <- diff(h_data$dttm[1:10])       #get interval after nas removed (remove any padded values)
            interval  <- median(intervals, na.rm=T)
            
            #calculate pct time open and closed
            pct_open   <- round(sum(h_data$sash=="open")/sum(h_data$sash %in% c("open", "closed")),3)
            pct_closed <- round(sum(h_data$sash=="closed")/sum(h_data$sash %in% c("open", "closed")),3)
            
            #determine weekday or weekend
            h_data$wday <- strftime(h_data$dttm,'%u') %in% c(1,2,3,4,5) 
            pct_open_wd <- round(sum(h_data$sash=="open" &  h_data$wday)/sum(h_data$wday & h_data$sash %in% c("open", "closed")),3)
            pct_open_we <- round(sum(h_data$sash=="open" & !h_data$wday)/sum(!h_data$wday & h_data$sash %in% c("open", "closed")),3)
            
            #determine run lengths (periods of continuous open or closed intervals)
            run_length                  <- data.frame(lengths=rle(h_data$sash)$lengths, values=rle(h_data$sash)$values)
            run_length$cumsum           <- cumsum(run_length$lengths)
            run_length$exceeds_5        <- run_length$values=="open" & run_length$lengths>(5*60/as.numeric(interval))
            run_length$exceeds_8        <- run_length$values=="open" & run_length$lengths>(8*60/as.numeric(interval))
            run_length$exceeds_24       <- run_length$values=="open" & run_length$lengths>(24*60/as.numeric(interval))
            
            run_length$int_exceeding_5  <- run_length$lengths - (5*60/as.numeric(interval)) #count intervals exceeding 5hrs
            run_length$int_exceeding_5[run_length$values %in% c("no data", "closed")]    <- 0
            run_length$int_exceeding_5[run_length$int_exceeding_5 <0]                    <- 0
            
            total_intervals <- sum(run_length$lengths[run_length$values %in% c("open","closed")])
            pct_open_over_5  <- round(sum(run_length$lengths[run_length$exceeds_5==TRUE])/total_intervals,3)
            pct_open_under_5 <- round(sum(run_length$lengths[run_length$exceeds_5==FALSE & run_length$values=="open"])/total_intervals,3)
                        
            total_openings         <- sum(run_length$values=="open", na.rm=T)
            
            if(total_openings==0){
                med_opening_hrs <- 0
                max_opening_hrs <- 0
                openings_over_5 <- 0
                openings_over_8 <- 0
                openings_over_24 <- 0
                pct_exceeding_5 <- 0
            }else{
                med_opening_hrs <- round(median(run_length$lengths[run_length$values=="open"], na.rm=T)*(as.numeric(interval)/60))
                max_opening_hrs <- round(max(run_length$lengths[run_length$values=="open"], na.rm=T)*(as.numeric(interval)/60))
                openings_over_5 <- round(sum(run_length$exceeds_5, na.rm=T))
                openings_over_8 <- round(sum(run_length$exceeds_8, na.rm=T))
                openings_over_24 <- round(sum(run_length$exceeds_24, na.rm=T))
                pct_exceeding_5 <- round(sum(run_length$int_exceeding_5, na.rm=T)/total_intervals,3)
            }
            
            full_summary <- data.frame(hood=h, interval, pct_na, 
                                       total_intervals, pct_open, pct_closed,
                                       min_v=min_v, median_v, mean_v, max_v, threshold,
                                       pct_open_over_5, pct_open_under_5, 
                                       total_openings, med_opening_hrs, max_opening_hrs, 
                                       openings_over_5, openings_over_8, openings_over_24, pct_exceeding_5
            )
            if(plot_ts & full_summary$pct_na!=1)
                
                plot_fume_hood(hood_sash_data=h_data, run_length, full_summary, file)
            
            return(full_summary)
        })
        hood_summary <- dplyr::rbind_all(hood_summary)
        
    })
    all_data_summary <- dplyr::rbind_all(all_data_summary)
    
    all_data_summary <- all_data_summary %>%
        mutate(score=openings_over_5*5 + openings_over_8*5 + openings_over_24*90) %>%
        mutate(ratio_mean_min_flow= round(mean_v/min_v,3))
    
    all_data_summary$ratio_mean_min_flow[all_data_summary$min_v %in% c(0,1)] <- 0
    return(all_data_summary)
    },error=function(e){
        print(paste("Error in function 'calculate_hood_metrics',",e))
    })
}