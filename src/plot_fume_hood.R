library(ggplot2)
library(scales)

plot_fume_hood <- function(hood_sash_data, run_length, full_summary){
    hood_sash_data$exceeds_5 <- NA
    hood_sash_data$exceeds_8 <- NA
    hood_sash_data$exceeds_24 <- NA
    
    # if there are open/closed sash periods...
    if(nrow(run_length)>2){
        if(sum(run_length$exceeds_5)>0){        #mark those where the sash is open more than 5 hours
            for(i in 1:sum(run_length$exceeds_5)){
                rle_row <- which(run_length$exceeds_5)[i]
                if(rle_row==1){ hood_sash_data$exceeds_5[1:run_length$cumsum[rle_row]] <- hood_sash_data$dttm[1:run_length$cumsum[rle_row]] 
                }else{          hood_sash_data$exceeds_5[(run_length$cumsum[rle_row-1]+1):run_length$cumsum[rle_row]] <- hood_sash_data$dttm[(run_length$cumsum[rle_row-1]+1):run_length$cumsum[rle_row]] }
            }
        }
        if(sum(run_length$exceeds_8)>0){        #mark those where the sash is open more than 8 hours
            for(i in 1:sum(run_length$exceeds_8)){
                rle_row <- which(run_length$exceeds_8)[i]
                if(rle_row==1){ hood_sash_data$exceeds_8[1:run_length$cumsum[rle_row]] <- hood_sash_data$dttm[1:run_length$cumsum[rle_row]] 
                }else{          hood_sash_data$exceeds_8[(run_length$cumsum[rle_row-1]+1):run_length$cumsum[rle_row]] <- hood_sash_data$dttm[(run_length$cumsum[rle_row-1]+1):run_length$cumsum[rle_row]] }            }
        }
        if(sum(run_length$exceeds_24)>0){       #mark those where the sash is open more than 24 hours
            for(i in 1:sum(run_length$exceeds_24)){
                rle_row <- which(run_length$exceeds_24)[i]
                if(rle_row==1){ hood_sash_data$exceeds_24[1:run_length$cumsum[rle_row]] <- hood_sash_data$dttm[1:run_length$cumsum[rle_row]] 
                }else{          hood_sash_data$exceeds_24[(run_length$cumsum[rle_row-1]+1):run_length$cumsum[rle_row]] <- hood_sash_data$dttm[(run_length$cumsum[rle_row-1]+1):run_length$cumsum[rle_row]] }            }
        }
    }#if
    hood_sash_data$color[hood_sash_data$sash=="open"]   <- "grey"
    hood_sash_data$color[hood_sash_data$sash=="closed"] <- "black"
    
    title_text <- paste(full_summary$hood,"\n File:",data_files[full_summary$file], 
                        "\n Interval:", full_summary$interval,
                        "min \n Total openings:", full_summary$total_openings, ". Max open duration (hrs): ",full_summary$max_opening_hrs,
                        "\n Number openings over 5 hrs:", full_summary$openings_over_5)
    trend <- ggplot(hood_sash_data)+
        geom_line(aes(x=dttm, y=h_data)) + 
        geom_point(aes(x=dttm, y=h_data), color=hood_sash_data$color) + 
        geom_point(aes(x=as.POSIXct(exceeds_5, origin="1970-01-01"), y=-0.1), color="yellow", lty="-")+
        geom_point(aes(x=as.POSIXct(exceeds_8, origin="1970-01-01"), y=-0.1), color="orange", lty="-")+
        geom_point(aes(x=as.POSIXct(exceeds_24, origin="1970-01-01"), y=-0.1), color="red", lty="-")+
        theme_minimal() + ylab("cfm")+
        ggtitle(title_text) + 
        scale_x_datetime(breaks = date_breaks("1 day"), labels=date_format("%y-%b-%d")) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    suppressWarnings(print(trend))
}#function