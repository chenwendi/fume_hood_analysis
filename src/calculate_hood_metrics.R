calculate_hood_metrics <- function(all_formatted_data, hoods_to_exclude, plot_ts=TRUE){
    # begin to loop through each set of fume hood data (each set corresponds to a csv file)
    all_data_summary <- lapply(1:length(all_formatted_data), FUN=function(c){
        print(c)
        ##### basic calculations, summary of the data included in a given file
        data_c <- all_formatted_data[[c]]
        min_date <- min(data_c$dttm)
        max_date <- max(data_c$dttm)
        range    <- max_date-min_date
        
        intervals <- data_c$dttm[1:10]- lag(data_c$dttm[1:10],1)
        interval <- median(intervals, na.rm=T)
        
        file_summary <- data.frame(file=data_files[c], min_date, max_date, days=round(range,2), interval)
        
        #### summary of individual fume hoods
        fume_hood_names <- setdiff(names(data_c),"dttm")
        hood_summary <- lapply(fume_hood_names, FUN=function(h){
            if(h %in% hoods_to_exclude)    return(NULL)     #don't evaluate non-relevant hoods
            print(h)
            c_index  <- which(names(data_c)==h)
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
            
            h_data$wday <- strftime(h_data$dttm,'%u') %in% c(1,2,3,4,5) #determine weekday or weekend
            pct_open_wd <- round(sum(h_data$hood[h_data$wday & !is.na(h_data$hood)]>=threshold)/sum(h_data$wday[!is.na(h_data$hood)]),3)
            pct_open_we <- round(sum(h_data$hood[!h_data$wday & !is.na(h_data$hood)]>=threshold)/sum(!h_data$wday[!is.na(h_data$hood)]),3)
            
            pct_open   <- round(sum(h_data$hood[!is.na(h_data$hood)]>=threshold)/length(h_data$hood[!is.na(h_data$hood)]),3)
            pct_closed <- round(sum(h_data$hood[!is.na(h_data$hood)]<threshold, na.rm=T)/length(h_data$hood[!is.na(h_data$hood)]),3)
            
            h_data$sash <- "no data"
            h_data$sash[h_data$hood>=threshold] <- "open"
            h_data$sash[h_data$hood<threshold]  <- "closed"
            run_length            <- data.frame(lengths=rle(h_data$sash)$lengths, values=rle(h_data$sash)$values)
            run_length$cumsum     <- cumsum(run_length$lengths)
            run_length$exceeds_5  <- run_length$values=="open" & run_length$lengths>(5*60/as.numeric(interval))
            run_length$exceeds_8  <- run_length$values=="open" & run_length$lengths>(8*60/as.numeric(interval))
            run_length$exceeds_24 <- run_length$values=="open" & run_length$lengths>(24*60/as.numeric(interval))
            
            total_intervals <- sum(run_length$lengths[run_length$values %in% c("open","closed")])
            pct_open        <- round(sum(run_length$lengths[run_length$values=="open"])/total_intervals,3)
            pct_closed      <- round(sum(run_length$lengths[run_length$values=="closed"])/total_intervals,3)
            pct_open_over_5  <- round(sum(run_length$lengths[run_length$values=="open" & run_length$lengths>(5*60/as.numeric(interval))])/total_intervals,3)
            pct_open_under_5 <- round(sum(run_length$lengths[run_length$values=="open" & run_length$lengths<=(5*60/as.numeric(interval))])/total_intervals,3)
                        
            total_openings         <- sum(run_length$values=="open", na.rm=T)
            total_openings_per_wk  <- round(sum(run_length$values=="open", na.rm=T)/(total_intervals/(7*24*60/as.numeric(interval))),3)
            if(total_openings==0){
                med_opening_hrs <- 0
                max_opening_hrs <- 0
                openings_over_5 <- 0
                openings_over_8 <- 0
                openings_over_24 <- 0
            }else{
                med_opening_hrs <- round(median(run_length$lengths[run_length$values=="open"], na.rm=T)*(as.numeric(interval)/60))
                max_opening_hrs <- round(max(run_length$lengths[run_length$values=="open"], na.rm=T)*(as.numeric(interval)/60))
                openings_over_5 <- round(sum(run_length$exceeds_5, na.rm=T))
                openings_over_8 <- round(sum(run_length$exceeds_8, na.rm=T))
                openings_over_24 <- round(sum(run_length$exceeds_24, na.rm=T))
            }
            
            full_summary <- data.frame(file_summary, hood=h, 
                                       min_v=min_v, median_v, mean_v, max_v, threshold,
                                       pct_open, pct_closed, pct_open_over_5, pct_open_under_5, total_intervals,
                                       total_openings, total_openings_per_wk, med_opening_hrs, max_opening_hrs, 
                                       openings_over_5, openings_over_8, openings_over_24
            )
            if(plot_ts)
                plot_fume_hood(hood_sash_data=h_data, run_length, full_summary)
            
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
}